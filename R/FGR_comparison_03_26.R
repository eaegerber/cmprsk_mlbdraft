# Fine–Gray w/ riskRegression: fit + predict + performance checks
library(riskRegression)
library(prodlim)
library(data.table)
library(parallel)

# Data prep
clean_df <- read.csv("R/cleaned_df2.csv")
df <- clean_df[clean_df$Signed == TRUE,]
df <- df[complete.cases(df),]
df <- df[df$Type %in% c("4Yr", "HS", "JC"),]
df <- df[df$Bats %in% c("B", "L", "R"),]
df <- df[df$Throws %in% c("L", "R"),]
df$Pos[which(df$Pos == "P" & df$Throws == "L")] <- "LHP"
df$Pos[which(df$Pos == "P" & df$Throws == "R")] <- "RHP"
df$newPOS[which(df$newPOS == "P" & df$Throws == "L")] <- "LHP"
df$newPOS[which(df$newPOS == "P" & df$Throws == "R")] <- "RHP"

df$Rnd <- as.factor(as.numeric(df$Rnd))

# 1) Make sure scaled vars are numeric vectors (scale() returns a matrix)
df$OvPck_sc <- as.numeric(scale(df$OvPck))
df$BSp_sc   <- as.numeric(scale(df$BSp))

# 2) Ensure factor types (droplevels helps)
df$Type      <- droplevels(factor(df$Type))
df$newPOS    <- droplevels(factor(df$newPOS))
df$Bats      <- droplevels(factor(df$Bats))
df$COVID_era <- droplevels(factor(df$COVID_era))

# 3) Make status a factor with explicit censoring level
#    (This avoids ambiguity about what "0" means inside Hist/FGR)
df$status_fg <- factor(df$status, levels=c(0,1,2), labels=c("cens","MLB","Retire"))

# 4) Build analysis data used by BOTH formulas, then drop NAs once
vars_mlb <- c("times","status_fg","OvPck_sc","BSp_sc","Type","newPOS","Age","Bats","COVID_era")
vars_ret <- c("times","status_fg","OvPck_sc","BSp_sc","Type","newPOS","Age")

df_fg_mlb <- df[vars_mlb]
df_fg_ret <- df[vars_ret]

df_fg_mlb <- droplevels(df_fg_mlb[complete.cases(df_fg_mlb), ])
df_fg_ret <- droplevels(df_fg_ret[complete.cases(df_fg_ret), ])

# -----------------------------
# 1) Fit Fine–Gray models (cause 1 and cause 2)
# -----------------------------

form_mlb_full <- Hist(times, status_fg) ~ OvPck_sc*BSp_sc*Type + OvPck_sc*newPOS + Age + Bats + COVID_era
form_ret_full <- Hist(times, status_fg) ~ OvPck_sc*BSp_sc*Type + OvPck_sc*BSp_sc + newPOS + Age

fg_mlb <- FGR(form_mlb_full, data=df_fg_mlb, cause="MLB")
fg_ret <- FGR(form_ret_full, data=df_fg_ret, cause="Retire")

# -----------------------------
# 2) CIF predictions
# -----------------------------
# Predict CIF at horizons:
horizons <- c(3,5,8,10)

# predictRisk() returns absolute risk (CIF) for the specified cause and times
risk_mlb <- predictRisk(fg_mlb, newdata=df[1:10,], times=horizons, cause=1)
risk_ret <- predictRisk(fg_ret, newdata=df[1:10,], times=horizons, cause=2)

print(risk_mlb)
print(risk_ret)

# -----------------------------
# 3) Performance: AUC, Brier, Calibration (with bootstrap CV and parallel)
# -----------------------------
# Use bootstrap cross-validation on the same data ("BootCv") to compare models.
# Score has built-in parallelization + progress bar options

# Compare a simpler MLB model vs full interaction MLB model:
fg_mlb_simple <- FGR(Hist(times,status) ~ OvPck_sc + BSp_sc + Type + newPOS + Age + Bats + COVID_era,
                     data=df, cause=1)

# Set up cores
ncores <- max(1, parallel::detectCores() - 1)

# ---- MLB (cause 1) ----
set.seed(1)
score_mlb <- Score(
  object = list("FG_simple"=fg_mlb_simple, "FG_full"=fg_mlb),
  formula = Hist(times,status) ~ 1,     # RHS used for IPCW censoring model in presence of covariates; ~1 is fine
  data    = df,
  cause   = 1,
  times   = horizons,
  metrics = c("auc","brier"),
  plots   = c("Calibration","ROC"),      # ROC = time-dependent ROC data
  summary = c("ibs","ipa"),              # integrated Brier, index of prediction accuracy
  split.method = "BootCv",
  B = 200,
  parallel = "multicore",
  ncpus = ncores,
  progress.bar = 3
)

print(score_mlb)
summary(score_mlb)

# Plot time-dependent AUC / Brier
plotAUC(score_mlb)
plotBrier(score_mlb)

# Calibration curves at each horizon
plotCalibration(score_mlb, times=3,  cause=1, legend=TRUE)
plotCalibration(score_mlb, times=5,  cause=1, legend=TRUE)
plotCalibration(score_mlb, times=8,  cause=1, legend=TRUE)
plotCalibration(score_mlb, times=10, cause=1, legend=TRUE)

# ---- Retire (cause 2) ----
# (Optional) also compare full vs simpler retire model
fg_ret_simple <- FGR(Hist(times,status) ~ OvPck_sc + BSp_sc + Type + newPOS + Age,
                     data=df, cause=2)

set.seed(1)
score_ret <- Score(
  object = list("FG_simple"=fg_ret_simple, "FG_full"=fg_ret),
  formula = Hist(times,status) ~ 1,
  data    = df,
  cause   = 2,
  times   = horizons,
  metrics = c("auc","brier"),
  plots   = c("Calibration","ROC"),
  summary = c("ibs","ipa"),
  split.method = "BootCv",
  B = 200,
  parallel = "multicore",
  ncpus = ncores,
  progress.bar = 3
)

print(score_ret)
summary(score_ret)

plotAUC(score_ret)
plotBrier(score_ret)

plotCalibration(score_ret, times=3,  cause=2, legend=TRUE)
plotCalibration(score_ret, times=5,  cause=2, legend=TRUE)
plotCalibration(score_ret, times=8,  cause=2, legend=TRUE)
plotCalibration(score_ret, times=10, cause=2, legend=TRUE)
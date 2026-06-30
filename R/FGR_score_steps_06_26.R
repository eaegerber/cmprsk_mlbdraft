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

# 4) Build one analysis data frame used by both FGR models
vars_fgr <- c("times","status_fg","OvPck_sc","BSp_sc","Type","newPOS","Age","Bats","COVID_era")

# 5) Create analysis frame used for FGR fitting
df_fgr <- df[, vars_fgr]

# Drop incomplete rows
df_fgr <- droplevels(df_fgr[complete.cases(df_fgr), ])

# 6) Sanity checks before fitting models
cat("\nFrame dimensions:\n")
print(dim(df_fgr))
cat("\nStatus table:\n")
print(table(df_fgr$status_fg, useNA = "ifany"))
cat("\nType levels:\n")
print(table(df_fgr$Type, useNA = "ifany"))
cat("\nnewPOS levels:\n")
print(table(df_fgr$newPOS, useNA = "ifany"))
cat("\nBats levels:\n")
print(table(df_fgr$Bats, useNA = "ifany"))
cat("\nCOVID_era levels:\n")
print(table(df_fgr$COVID_era, useNA = "ifany"))

# -----------------------------
# 1) FGR setup and simple tests
# -----------------------------

# Numeric status for FGR:
# 0 = censored, 1 = MLB, 2 = Retire
df_fgr$status <- as.integer(df[rownames(df_fgr), "status"])

# FGR can be sensitive to times == 0, so create a computational time variable
df_fgr$times_fgr <- ifelse(df_fgr$times <= 0, 1e-6, df_fgr$times)

# Simple model calls for testing purposes
fg_mlb_simple <- FGR(
  Hist(times_fgr, status) ~ OvPck_sc + BSp_sc + Type + newPOS + Age + Bats + COVID_era,
  data = df_fgr,
  cause = 1
)

summary(fg_mlb_simple)

# Don't include COVID_era for retire model; simply not enough time to retire post-COVID
fg_ret_simple <- FGR(
  Hist(times_fgr, status) ~ OvPck_sc + BSp_sc + Type + newPOS + Age + Bats,
  data = df_fgr,
  cause = 2
)

summary(fg_ret_simple)

# -----------------------------
# 2) Fitting selected models from model_selection_03_26.R
# -----------------------------

X_mlb_full <- model.matrix(
  ~ OvPck_sc*BSp_sc*Type + OvPck_sc*newPOS + Age + Bats + COVID_era,
  data = df_fgr
)[, -1, drop = FALSE]

X_ret_full <- model.matrix(
  ~ OvPck_sc*BSp_sc*Type + OvPck_sc*BSp_sc + newPOS + Age,
  data = df_fgr
)[, -1, drop = FALSE]


mlb_full_name_map <- data.frame(
  original_name = colnames(X_mlb_full),
  fgr_name = make.names(colnames(X_mlb_full), unique = TRUE)
)

ret_full_name_map <- data.frame(
  original_name = colnames(X_ret_full),
  fgr_name = make.names(colnames(X_ret_full), unique = TRUE)
)

X_mlb_full <- as.data.frame(X_mlb_full)
names(X_mlb_full) <- mlb_full_name_map$fgr_name

X_ret_full <- as.data.frame(X_ret_full)
names(X_ret_full) <- ret_full_name_map$fgr_name

df_mlb_full <- cbind(
  df_fgr[, c("times_fgr", "status")],
  X_mlb_full
)

df_ret_full <- cbind(
  df_fgr[, c("times_fgr", "status")],
  X_ret_full
)

form_mlb_full <- as.formula(
  paste(
    "Hist(times_fgr, status) ~",
    paste(names(X_mlb_full), collapse = " + ")
  )
)

form_ret_full <- as.formula(
  paste(
    "Hist(times_fgr, status) ~",
    paste(names(X_ret_full), collapse = " + ")
  )
)

# Fit selected models
fg_mlb_selected <- FGR(
  form_mlb_full,
  data = df_mlb_full,
  cause = 1
)

summary(fg_mlb_selected)

fg_ret_selected <- FGR(
  form_ret_full,
  data = df_ret_full,
  cause = 2
)

summary(fg_ret_selected)


# -----------------------------
# 3) Prediction smoke test before Score()
# -----------------------------

horizons <- c(3, 5, 8, 10)

# For MLB scoring/prediction, include both original variables and selected-model matrix columns
df_score_mlb <- cbind(
  df_fgr,
  X_mlb_full
)

# For retire scoring/prediction, include both original variables and selected-model matrix columns
df_score_ret <- cbind(
  df_fgr,
  X_ret_full
)

# Smoke-test predictions for first 10 rows
risk_mlb_simple <- predictRisk(
  fg_mlb_simple,
  newdata = df_score_mlb[1:10, ],
  times = horizons
)

risk_mlb_selected <- predictRisk(
  fg_mlb_selected,
  newdata = df_score_mlb[1:10, ],
  times = horizons
)

risk_ret_simple <- predictRisk(
  fg_ret_simple,
  newdata = df_score_ret[1:10, ],
  times = horizons
)

risk_ret_selected <- predictRisk(
  fg_ret_selected,
  newdata = df_score_ret[1:10, ],
  times = horizons
)

cat("\nMLB simple predictions:\n")
print(risk_mlb_simple)

cat("\nMLB selected predictions:\n")
print(risk_mlb_selected)

cat("\nRetire simple predictions:\n")
print(risk_ret_simple)

cat("\nRetire selected predictions:\n")
print(risk_ret_selected)

# -----------------------------
# 4A) Score comparison: apparent performance first
# No bootstrap CV yet
# -----------------------------

horizons <- c(3, 5, 8, 10)

score_mlb_apparent <- Score(
  object = list(
    "MLB_simple"   = fg_mlb_simple,
    "MLB_selected" = fg_mlb_selected
  ),
  formula = Hist(times_fgr, status) ~ 1,
  data = df_score_mlb,
  cause = 1,
  times = horizons,
  metrics = c("auc", "brier"),
  plots = c("Calibration"),
  summary = c("ibs", "ipa")
)

print(score_mlb_apparent)
summary(score_mlb_apparent)

# -----------------------------
# 4A) Score comparison: apparent performance first
# No bootstrap CV yet
# -----------------------------

horizons <- c(3, 5, 8, 10)

score_mlb_apparent <- Score(
  object = list(
    "MLB_simple"   = fg_mlb_simple,
    "MLB_selected" = fg_mlb_selected
  ),
  formula = Hist(times_fgr, status) ~ 1,
  data = df_score_mlb,
  cause = 1,
  times = horizons,
  metrics = c("auc", "brier"),
  plots = c("Calibration"),
  summary = c("ibs", "ipa")
)

print(score_mlb_apparent)
summary(score_mlb_apparent)

# -----------------------------
# 5) Score comparison: Retire simple vs selected
# Apparent performance first
# -----------------------------

score_ret_apparent <- Score(
  object = list(
    "Ret_simple"   = fg_ret_simple,
    "Ret_selected" = fg_ret_selected
  ),
  formula = Hist(times_fgr, status) ~ 1,
  data = df_score_ret,
  cause = 2,
  times = horizons,
  metrics = c("auc", "brier"),
  plots = c("Calibration"),
  summary = c("ibs", "ipa")
)

print(score_ret_apparent)
summary(score_ret_apparent)

# -----------------------------
# Save apparent Score results
# -----------------------------

save(
  score_mlb_apparent,
  score_ret_apparent,
  file = "FGR_apparent_score_results.RData"
)

# -----------------------------
# Calibration plots: apparent performance
# -----------------------------

plotCalibration(score_mlb_apparent, times = 3, cause = 1, legend = TRUE, cens.method = "local")
plotCalibration(score_mlb_apparent, times = 5, cause = 1, legend = TRUE, cens.method = "local")
plotCalibration(score_mlb_apparent, times = 8, cause = 1, legend = TRUE, cens.method = "local")
plotCalibration(score_mlb_apparent, times = 10, cause = 1, legend = TRUE, cens.method = "local")

plotCalibration(score_ret_apparent, times = 3, cause = 2, legend = TRUE, cens.method = "local")
plotCalibration(score_ret_apparent, times = 5, cause = 2, legend = TRUE, cens.method = "local")
plotCalibration(score_ret_apparent, times = 8, cause = 2, legend = TRUE, cens.method = "local")
plotCalibration(score_ret_apparent, times = 10, cause = 2, legend = TRUE, cens.method = "local")

# -----------------------------
# Bootstrap CV smoke test: MLB only
# -----------------------------

set.seed(42)

score_mlb_boot_test <- Score(
  object = list(
    "MLB_simple"   = fg_mlb_simple,
    "MLB_selected" = fg_mlb_selected
  ),
  formula = Hist(times_fgr, status) ~ 1,
  data = df_score_mlb,
  cause = 1,
  times = horizons,
  metrics = c("auc", "brier"),
  plots = c("Calibration"),
  summary = c("ibs", "ipa"),
  split.method = "BootCv",
  B = 5,
  parallel = "no",
  progress.bar = 3
)

print(score_mlb_boot_test)
summary(score_mlb_boot_test)

# -----------------------------
# Bootstrap CV smoke test: Retire only
# -----------------------------

set.seed(42)

score_ret_boot_test <- Score(
  object = list(
    "Ret_simple"   = fg_ret_simple,
    "Ret_selected" = fg_ret_selected
  ),
  formula = Hist(times_fgr, status) ~ 1,
  data = df_score_ret,
  cause = 2,
  times = horizons,
  metrics = c("auc", "brier"),
  plots = c("Calibration"),
  summary = c("ibs", "ipa"),
  split.method = "BootCv",
  B = 5,
  parallel = "no",
  progress.bar = 3
)

print(score_ret_boot_test)
summary(score_ret_boot_test)

save(
  score_mlb_apparent,
  score_ret_apparent,
  score_mlb_boot_test,
  score_ret_boot_test,
  file = "FGR_score_results_smoke_tests.RData"
)

# -----------------------------
# Larger bootstrap CV validation
# -----------------------------

B_SCORE_FINAL <- 100

set.seed(42)

score_mlb_boot_final <- Score(
  object = list(
    "MLB_simple"   = fg_mlb_simple,
    "MLB_selected" = fg_mlb_selected
  ),
  formula = Hist(times_fgr, status) ~ 1,
  data = df_score_mlb,
  cause = 1,
  times = horizons,
  metrics = c("auc", "brier"),
  summary = c("ibs", "ipa"),
  split.method = "BootCv",
  B = B_SCORE_FINAL,
  parallel = "no",
  progress.bar = 3
)

score_ret_boot_final <- Score(
  object = list(
    "Ret_simple"   = fg_ret_simple,
    "Ret_selected" = fg_ret_selected
  ),
  formula = Hist(times_fgr, status) ~ 1,
  data = df_score_ret,
  cause = 2,
  times = horizons,
  metrics = c("auc", "brier"),
  summary = c("ibs", "ipa"),
  split.method = "BootCv",
  B = B_SCORE_FINAL,
  parallel = "no",
  progress.bar = 3
)

save(
  score_mlb_boot_final,
  score_ret_boot_final,
  file = paste0("FGR_score_results_B", B_SCORE_FINAL, ".RData")
)

print(score_mlb_boot_final)
summary(score_mlb_boot_final)

print(score_ret_boot_final)
summary(score_ret_boot_final)
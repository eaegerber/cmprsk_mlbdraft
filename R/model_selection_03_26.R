#Analysis Using Cleaned Data for Paper
library(cmprsk)
library(riskRegression)
library(prodlim)

#Read in cleaned data
clean_df <- read.csv("R/cleaned_df2.csv")
clean_df_new <- clean_df[clean_df$Signed == TRUE,]
clean_df_new <- clean_df_new[complete.cases(clean_df_new),]
clean_df_new <- clean_df_new[clean_df_new$Type %in% c("4Yr", "HS", "JC"),]
clean_df_new <- clean_df_new[clean_df_new$Bats %in% c("B", "L", "R"),]
clean_df_new <- clean_df_new[clean_df_new$Throws %in% c("L", "R"),]
clean_df_new$Pos[which(clean_df_new$Pos == "P" & clean_df_new$Throws == "L")] <- "LHP"
clean_df_new$Pos[which(clean_df_new$Pos == "P" & clean_df_new$Throws == "R")] <- "RHP"
clean_df_new$newPOS[which(clean_df_new$newPOS == "P" & clean_df_new$Throws == "L")] <- "LHP"
clean_df_new$newPOS[which(clean_df_new$newPOS == "P" & clean_df_new$Throws == "R")] <- "RHP"
attach(clean_df_new)

#Non-parametric Competing Risks using cmprsk package
word_status <- ifelse(status==1, "MLB", ifelse(status==2, "Retire", 0))
cif_mlb <- cuminc(ftime = times, fstatus = word_status, group=Type)
plot(cif_mlb, col = 1:6, xlab = "Years", main = "Non-Parametric CIFs by Type", lwd = 2)
cif_mlb$Tests

setEPS()
postscript("NP_CIF_Type.eps", width=6, height=5)
plot(cif_mlb, col = 1:6, xlab = "Years", main = "Non-Parametric CIFs by Type", lwd = 2)
dev.off()

#There is at least one difference between the three types for both types of "deaths"

#What about just 4YR vs. HS
cif_mlb2 <- cuminc(ftime = times[which(Type != "JC")], fstatus = status[which(Type != "JC")], group=Type[which(Type != "JC")])
plot(cif_mlb2, col = 1:4, xlab = "Years", main = "Risk of Reaching Majors (1) or Retiring (2) based on Type")
cif_mlb2$Tests

#There is a difference, however there seems to be a violation of the proportional hazards based on the risk of reaching the majors
#However, looking at other features, such as Pitchers vs. Batters, the proportional assumption isn't too unreasonable:
cif_mlb3 <- cuminc(ftime = times, fstatus = word_status, group=Pitch)
plot(cif_mlb3, col = 1:4, xlab = "Years", main = "Non-Parametric CIFs by Pitch/Bat", lwd = 2)
cif_mlb3$Tests

#Well, maybe if we use the more granular position feature, there might be issues:
cif_mlb4 <- cuminc(ftime = times, fstatus = word_status, group=newPOS)
plot(cif_mlb4, col = c(6:10, rep(0, 5)), xlab = "Years", main = "Non-Parametric CIFs by Pos", lwd = 2, ylim = c(0,1))
cif_mlb4$Tests

#Actually not too bad considering, but technically the assumption seems violated
cs.cuminc <- function(x,cause="1"){
  if (!is.null(x$Tests)) 
    x <- x[names(x) != "Tests"]
  which.out <- which(unlist(strsplit(names(x), " "))[seq(2,length(names(x))*2,2)]!=cause)
  x[which.out] <- NULL
  class(x) <- "cuminc"
  return(x)
}

x.2 <- cs.cuminc(cif_mlb4, cause="MLB")
plot(x.2, col = c(6:10, rep(0, 5)), xlab = "Years", main = "Non-Parametric CIFs by Pos", lwd = 2, ylim = c(0,.5))

setEPS()
postscript("NP_CIF_Pos_MLB.eps", width=6, height=5)
plot(x.2, col = c(6:10, rep(0, 5)), xlab = "Years", main = "Non-Parametric CIFs by Pos", lwd = 2, ylim = c(0,.5))
dev.off()

#What about just batter vs LHP and RHP
temp_POS <- ifelse(newPOS == "LHP", "LHP", ifelse(newPOS == "RHP", "RHP", "Bat"))
cif_mlb5 <- cuminc(ftime = times, fstatus = word_status, group=temp_POS)
plot(cif_mlb5, col = 1:10, xlab = "Years", main = "Non-Parametric CIFs by Pitch/Bat", lwd = 2)
cif_mlb5$Tests

setEPS()
postscript("NP_CIF_PB.eps", width=6, height=5)
plot(cif_mlb5, col = 1:10, xlab = "Years", main = "Non-Parametric CIFs by Pitch/Bat", lwd = 2)
dev.off()

#clean_df$Rnd[which(clean_df$Rnd == "1s")] <- "1.5"
clean_df_new$Rnd <- as.factor(as.numeric(clean_df_new$Rnd))

#Using crr from cmprsk so that I can check the Schoenfeld residuals (not sure how to get those from other packages)
#It is also convenient for experimenting with model selection, since I can just update the model.matrix and then all models are fit the same way:
scale_df <- clean_df_new
scale_df$OvPck <- scale(clean_df_new$OvPck)
scale_df$BSp <- scale(clean_df_new$BSp)

# Forward Model Selection using Log-Likelihood, likelihood ratio test (df), and p-values
# Reach MLB
cov0 <- model.matrix(~OvPck*BSp*Type + OvPck*newPOS*BSp + Age + Bats + COVID_era, data=scale_df)[,-1]
crr.model0 <- crr(times, status, cov1=cov0, failcode=1)
summary(crr.model0)

# Retire
cov02 <- model.matrix(~OvPck*BSp*Type + OvPck*BSp + newPOS + Age, data=scale_df)[,-1]
crr.model02 <- crr(times, status, cov1=cov02, failcode=2)
summary(crr.model02)

## Check stability of models using bootstrap
## Heavy three-way interactions reduce interpretability and may be overfitting
## Hard to do traditional cross validation with competing risks, but bootstrap can give us a sense of stability of the model
set.seed(123)

fit_crr <- function(dat, failcode, formula, status_col="status", time_col="times"){
  X <- model.matrix(formula, data=dat)[,-1, drop=FALSE]
  fit <- crr(ftime = dat[[time_col]], fstatus = dat[[status_col]], cov1 = X, failcode = failcode)
  fit$coef
}

B <- 200 # number of bootstrap samples
years <- sort(unique(scale_df$Year)) # cluster by year to preserve temporal structure and potential cohort effects

# original fit to get coefficient names (for alignment)
cn <- names(crr.model0$coef)

pb <- txtProgressBar(min = 0, max = B, style = 3)
t0 <- Sys.time()

boot_mat <- matrix(NA_real_, nrow=B, ncol=length(cn))
colnames(boot_mat) <- cn

for (b in 1:B) {
  sampled_years <- sample(years, size=length(years), replace=TRUE)
  boot_dat <- do.call(rbind, lapply(sampled_years, function(y) scale_df[scale_df$Year == y, ]))

  coefs <- tryCatch({
    Xb <- model.matrix(~OvPck*BSp*Type + OvPck*newPOS*BSp + Age + Bats + COVID_era, data=boot_dat)[,-1, drop=FALSE]
    fitb <- crr(boot_dat$times, boot_dat$status, cov1=Xb, failcode=1)
    fitb$coef
  }, error = function(e) NA)

  if (!all(is.na(coefs))) boot_mat[b, names(coefs)] <- coefs

  # progress + ETA
  setTxtProgressBar(pb, b)
  if (b %% 25 == 0) {
    elapsed <- as.numeric(difftime(Sys.time(), t0, units="secs"))
    eta <- (elapsed / b) * (B - b)
    cat(sprintf("\nBootstrap %d/%d | elapsed: %.1f min | ETA: %.1f min\n",
                b, B, elapsed/60, eta/60))
  }
}
close(pb)

summ_stability <- function(boot_mat, coef_name){
  x <- boot_mat[, coef_name]
  x <- x[!is.na(x)]
  c(
    n = length(x),
    mean = mean(x),
    sd = sd(x),
    q025 = quantile(x, 0.025),
    q975 = quantile(x, 0.975),
    sign_consistency = max(mean(x>0), mean(x<0)),  # close to 1 is good
    prob_gt0 = mean(x>0)
  )
}

terms_to_check <- c("OvPck:BSp:TypeHS", "OvPck:BSp:newPOSLHP")
t(sapply(terms_to_check, \(nm) summ_stability(boot_mat, nm)))
















#### Once I think the model selection is done, check for proportionality
# --- Proportional subdistribution hazards test (Fine–Gray) ---
# install.packages("crrSC")
library(crrSC)

# MLB model
X_mlb <- model.matrix(~OvPck_sc*BSp_sc*Type + OvPck_sc*newPOS*BSp_sc + Age + Bats + COVID_era, data=df)[,-1, drop=FALSE]

# psh.test needs: time, fstatus, z (covariate matrix)
# D indicates which cause is the event of interest; for MLB (cause 1) use D=c(1,1) with fstatus coded 0/1/2
# tf controls the time-function basis for time-varying effects (non-PH). Default in docs is cbind(t, t^2)

# Global test (all covariates together)
psh_global_mlb <- psh.test(time=df$times, fstatus=df$status, z=X_mlb,
                          D=c(1,1),
                          tf=function(t) cbind(log(pmax(t,1e-6)), (log(pmax(t,1e-6)))^2))

print(psh_global_mlb)

# Targeted tests for specific blocks if global test is significant (e.g., Type, newPOS, OvPck, BSp):
cols_type   <- grep("Type", colnames(X_mlb), fixed=TRUE)
cols_newpos <- grep("newPOS", colnames(X_mlb), fixed=TRUE)
cols_pick   <- grep("^OvPck_sc$|^OvPck_sc:", colnames(X_mlb))
cols_bsp    <- grep("^BSp_sc$|:BSp_sc", colnames(X_mlb))

psh_type_mlb   <- psh.test(df$times, df$status, X_mlb[, cols_type, drop=FALSE],   D=c(1,1),
                           tf=function(t) cbind(log(pmax(t,1e-6)), (log(pmax(t,1e-6)))^2))
psh_newpos_mlb <- psh.test(df$times, df$status, X_mlb[, cols_newpos, drop=FALSE], D=c(1,1),
                           tf=function(t) cbind(log(pmax(t,1e-6)), (log(pmax(t,1e-6)))^2))
psh_pick_mlb   <- psh.test(df$times, df$status, X_mlb[, cols_pick, drop=FALSE],   D=c(1,1),
                           tf=function(t) cbind(log(pmax(t,1e-6)), (log(pmax(t,1e-6)))^2))
psh_bsp_mlb    <- psh.test(df$times, df$status, X_mlb[, cols_bsp, drop=FALSE],    D=c(1,1),
                           tf=function(t) cbind(log(pmax(t,1e-6)), (log(pmax(t,1e-6)))^2))

list(type=psh_type_mlb, newPOS=psh_newpos_mlb, OvPck=psh_pick_mlb, BSp=psh_bsp_mlb)
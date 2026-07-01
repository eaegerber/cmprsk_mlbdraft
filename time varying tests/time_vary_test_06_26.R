## Checking for Time-Varying Effects
## Bruce's comment: Can you consider interactions with time to “relax” the proportional hazards assumption(with t^k, log t)? I have seen this done as a way to allow time-varying effects.  You could also choose only certain variables to have effects that vary over time and thus find a Fine and Gray model that works.

# Use same analysis frame from FGR_score_steps_06_26.R
load("./ModelValidation.RData")

df_tv <- df_fgr

# 0 = censored, 1 = MLB, 2 = Retire
df_tv$status <- as.integer(df_tv$status)
table(df_tv$status)

# Avoid log(0)
df_tv$times_tv <- ifelse(df_tv$times_fgr <= 0, 1e-6, df_tv$times_fgr)
hist(df_tv$times_tv)


# -----------------------------
# 1) Re-fit selected baseline models with crr() as found in model_selection_03_26.R
# -----------------------------
library(cmprsk)

X_mlb_base <- model.matrix(
  ~ OvPck_sc*BSp_sc*Type + OvPck_sc*newPOS + Age + Bats + COVID_era,
  data = df_tv
)[, -1, drop = FALSE]

crr_mlb_base <- crr(
  ftime = df_tv$times_tv,
  fstatus = df_tv$status,
  cov1 = X_mlb_base,
  failcode = 1,
  cencode = 0
)

summary(crr_mlb_base)

X_ret_base <- model.matrix(
  ~ OvPck_sc*BSp_sc*Type + OvPck_sc*BSp_sc + newPOS + Age,
  data = df_tv
)[, -1, drop = FALSE]

crr_ret_base <- crr(
  ftime = df_tv$times_tv,
  fstatus = df_tv$status,
  cov1 = X_ret_base,
  failcode = 2,
  cencode = 0
)

summary(crr_ret_base)



# -----------------------------
# 2) Helper functions for time variying checks
# -----------------------------


make_log_tf <- function(q) {
  function(t) {
    logt <- log(pmax(t, 1e-6))
    matrix(rep(logt, q), ncol = q)
  }
}

fit_tv_block <- function(data, base_formula, tv_formula, failcode, label) {
  
  # Base model covariates
  X_base <- model.matrix(base_formula, data = data)[, -1, drop = FALSE]
  
  # Covariates whose effects are allowed to vary with log(time)
  X_tv <- model.matrix(tv_formula, data = data)[, -1, drop = FALSE]
  q <- ncol(X_tv)
  
  # Rename time-varying columns clearly
  colnames(X_tv) <- paste0(colnames(X_tv), "_x_logtime")
  
  fit_base <- crr(
    ftime = data$times_tv,
    fstatus = data$status,
    cov1 = X_base,
    failcode = failcode,
    cencode = 0
  )
  
  fit_tv <- crr(
    ftime = data$times_tv,
    fstatus = data$status,
    cov1 = X_base,
    cov2 = X_tv,
    tf = make_log_tf(q),
    failcode = failcode,
    cencode = 0
  )
  
  lrt <- 2 * (fit_tv$loglik - fit_base$loglik)
  p_lrt <- pchisq(lrt, df = q, lower.tail = FALSE)
  
  cat("\n==============================\n")
  cat(label, "\n")
  cat("==============================\n")
  cat("Time-varying terms tested:", q, "\n")
  cat("Base log pseudo-likelihood:", fit_base$loglik, "\n")
  cat("TV log pseudo-likelihood:", fit_tv$loglik, "\n")
  cat("LRT statistic:", lrt, "\n")
  cat("df:", q, "\n")
  cat("LRT p-value:", p_lrt, "\n\n")
  
  cat("Time-varying coefficients:\n")
  print(tail(summary(fit_tv)$coef, q))
  
  invisible(list(
    label = label,
    fit_base = fit_base,
    fit_tv = fit_tv,
    q = q,
    lrt = lrt,
    p_lrt = p_lrt,
    tv_coef = tail(summary(fit_tv)$coef, q)
  ))
}

mlb_base_formula <- ~ OvPck_sc*BSp_sc*Type + OvPck_sc*newPOS + Age + Bats + COVID_era

ret_base_formula <- ~ OvPck_sc*BSp_sc*Type + OvPck_sc*BSp_sc + newPOS + Age



# -----------------------------
# 3) Run checks, one block at a time
# -----------------------------

## Type varies with time (most likely time varying effect)
tv_mlb_type <- fit_tv_block(
  data = df_tv,
  base_formula = mlb_base_formula,
  tv_formula = ~ Type,
  failcode = 1,
  label = "MLB: Type x log(time)"
)

tv_ret_type <- fit_tv_block(
  data = df_tv,
  base_formula = ret_base_formula,
  tv_formula = ~ Type,
  failcode = 2,
  label = "Retire: Type x log(time)"
)


## Overall Pick
tv_mlb_ovpck <- fit_tv_block(
  data = df_tv,
  base_formula = mlb_base_formula,
  tv_formula = ~ OvPck_sc,
  failcode = 1,
  label = "MLB: Overall pick x log(time)"
)

tv_ret_ovpck <- fit_tv_block(
  data = df_tv,
  base_formula = ret_base_formula,
  tv_formula = ~ OvPck_sc,
  failcode = 2,
  label = "Retire: Overall pick x log(time)"
)

## Postition Group
tv_mlb_newpos <- fit_tv_block(
  data = df_tv,
  base_formula = mlb_base_formula,
  tv_formula = ~ newPOS,
  failcode = 1,
  label = "MLB: newPOS x log(time)"
)

tv_ret_newpos <- fit_tv_block(
  data = df_tv,
  base_formula = ret_base_formula,
  tv_formula = ~ newPOS,
  failcode = 2,
  label = "Retire: newPOS x log(time)"
)

## Bonus as a proportion of Slot (BSp)
tv_mlb_bsp <- fit_tv_block(
  data = df_tv,
  base_formula = mlb_base_formula,
  tv_formula = ~ BSp_sc,
  failcode = 1,
  label = "MLB: Bonus-slot percentage x log(time)"
)

tv_ret_bsp <- fit_tv_block(
  data = df_tv,
  base_formula = ret_base_formula,
  tv_formula = ~ BSp_sc,
  failcode = 2,
  label = "Retire: Bonus-slot percentage x log(time)"
)

## Summary Table
tv_summary <- data.frame(
  model = c(
    "MLB",
    "MLB",
    "MLB",
    "MLB",
    "Retire",
    "Retire",
    "Retire",
    "Retire"
  ),
  tv_block = c(
    "Type x log(time)",
    "OvPck x log(time)",
    "newPOS x log(time)",
    "BSp x log(time)",
    "Type x log(time)",
    "OvPck x log(time)",
    "newPOS x log(time)",
    "BSp x log(time)"
  ),
  df = c(
    tv_mlb_type$q,
    tv_mlb_ovpck$q,
    tv_mlb_newpos$q,
    tv_mlb_bsp$q,
    tv_ret_type$q,
    tv_ret_ovpck$q,
    tv_ret_newpos$q,
    tv_ret_bsp$q
  ),
  LRT = c(
    tv_mlb_type$lrt,
    tv_mlb_ovpck$lrt,
    tv_mlb_newpos$lrt,
    tv_mlb_bsp$lrt,
    tv_ret_type$lrt,
    tv_ret_ovpck$lrt,
    tv_ret_newpos$lrt,
    tv_ret_bsp$lrt
  ),
  p_value = c(
    tv_mlb_type$p_lrt,
    tv_mlb_ovpck$p_lrt,
    tv_mlb_newpos$p_lrt,
    tv_mlb_bsp$p_lrt,
    tv_ret_type$p_lrt,
    tv_ret_ovpck$p_lrt,
    tv_ret_newpos$p_lrt,
    tv_ret_bsp$p_lrt
  )
)

tv_summary$padj_BH <- p.adjust(tv_summary$p_value, method = "BH")

print(tv_summary)

write.csv(tv_summary, "time_varying_sensitivity_summary.csv", row.names = FALSE)

save(
  tv_mlb_type,
  tv_mlb_ovpck,
  tv_mlb_newpos,
  tv_ret_type,
  tv_ret_ovpck,
  tv_ret_bsp,
  tv_ret_newpos,
  tv_summary,
  file = "time_varying_sensitivity_results.RData"
)

# Several are time varying, especially Type; investigate direction



# -----------------------------
# 4) Investigate coefficients
# -----------------------------

print_tv <- function(obj) {
  cat("\n\n==============================\n")
  cat(obj$label, "\n")
  cat("==============================\n")
  print(obj$tv_coef)
}

print_tv(tv_mlb_type)
print_tv(tv_mlb_ovpck)
print_tv(tv_mlb_bsp)
print_tv(tv_mlb_newpos)

print_tv(tv_ret_type)
print_tv(tv_ret_ovpck)
print_tv(tv_ret_bsp)
print_tv(tv_ret_newpos)



# -----------------------------
# 5) Combined time-varying sensitivity model helper
# -----------------------------

fit_tv_combined <- function(data, base_formula, tv_formula, failcode, label) {
  
  X_base <- model.matrix(base_formula, data = data)[, -1, drop = FALSE]
  
  X_tv <- model.matrix(tv_formula, data = data)[, -1, drop = FALSE]
  q <- ncol(X_tv)
  colnames(X_tv) <- paste0(colnames(X_tv), "_x_logtime")
  
  fit_base <- crr(
    ftime = data$times_tv,
    fstatus = data$status,
    cov1 = X_base,
    failcode = failcode,
    cencode = 0
  )
  
  fit_tv <- crr(
    ftime = data$times_tv,
    fstatus = data$status,
    cov1 = X_base,
    cov2 = X_tv,
    tf = make_log_tf(q),
    failcode = failcode,
    cencode = 0
  )
  
  lrt <- 2 * (fit_tv$loglik - fit_base$loglik)
  p_lrt <- pchisq(lrt, df = q, lower.tail = FALSE)
  
  cat("\n==============================\n")
  cat(label, "\n")
  cat("==============================\n")
  cat("Time-varying terms tested:", q, "\n")
  cat("Base log pseudo-likelihood:", fit_base$loglik, "\n")
  cat("TV log pseudo-likelihood:", fit_tv$loglik, "\n")
  cat("LRT statistic:", lrt, "\n")
  cat("df:", q, "\n")
  cat("LRT p-value:", p_lrt, "\n\n")
  
  cat("Time-varying coefficients:\n")
  print(tail(summary(fit_tv)$coef, q))
  
  invisible(list(
    label = label,
    fit_base = fit_base,
    fit_tv = fit_tv,
    q = q,
    lrt = lrt,
    p_lrt = p_lrt,
    tv_coef = tail(summary(fit_tv)$coef, q)
  ))
}

## MLB combined TV sensitivity

tv_mlb_combined <- fit_tv_combined(
  data = df_tv,
  base_formula = mlb_base_formula,
  tv_formula = ~ Type + OvPck_sc,
  failcode = 1,
  label = "MLB combined: Type + OvPck x log(time)"
)

## Retire combined TV sensitivity

tv_ret_combined <- fit_tv_combined(
  data = df_tv,
  base_formula = ret_base_formula,
  tv_formula = ~ Type + OvPck_sc + newPOS,
  failcode = 2,
  label = "Retire combined: Type + OvPck + newPOS x log(time)"
)

# BSp showed no additional time-varying contribution after accounting for Type and OvPck, so it was dropped from the reduced time-varying sensitivity models. For the retirement model, however, adding newPOS × log(time) significantly improved fit beyond Type × log(time) and OvPck × log(time) (LRT = 24.1, df = 4, p = 7.6e-05), so newPOS was retained in the retire time-varying sensitivity model.

save(
  tv_mlb_combined,
  tv_ret_combined,
  file = "time_varying_sensitivity_results_combined.RData"
)

tv_combined_summary <- data.frame(
  model = c("MLB", "Retire"),
  tv_terms = c(
    "Type + OvPck",
    "Type + OvPck + newPOS"
  ),
  df = c(
    tv_mlb_combined$q,
    tv_ret_combined$q
  ),
  LRT = c(
    tv_mlb_combined$lrt,
    tv_ret_combined$lrt
  ),
  p_value = c(
    tv_mlb_combined$p_lrt,
    tv_ret_combined$p_lrt
  )
)

print(tv_combined_summary)

write.csv(
  tv_combined_summary,
  "time_varying_sensitivity_combined_summary.csv",
  row.names = FALSE
)

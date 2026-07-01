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

## Effects seem important, so let's try to interpret them
# -----------------------------
# 6) Time-specific effects from crr time-varying models
# -----------------------------

make_time_effect_table <- function(tv_obj, main_terms, tv_prefixes, times = c(1, 3, 5, 8, 10)) {
  
  coefs <- tv_obj$fit_tv$coef
  
  out <- list()
  
  for (i in seq_along(main_terms)) {
    
    main_term <- main_terms[i]
    tv_prefix <- tv_prefixes[i]
    
    beta <- coefs[main_term]
    
    tv_name <- grep(
      paste0("^", tv_prefix, "_x_logtime\\*tf"),
      names(coefs),
      value = TRUE
    )
    
    if (length(tv_name) != 1) {
      stop(paste("Could not uniquely find time-varying term for:", tv_prefix))
    }
    
    gamma <- coefs[tv_name]
    
    tmp <- data.frame(
      term = main_term,
      time = times,
      beta_main = as.numeric(beta),
      gamma_logtime = as.numeric(gamma),
      coef_at_time = as.numeric(beta + gamma * log(times)),
      HR_at_time = as.numeric(exp(beta + gamma * log(times)))
    )
    
    out[[i]] <- tmp
  }
  
  do.call(rbind, out)
}

mlb_time_effects <- make_time_effect_table(
  tv_obj = tv_mlb_combined,
  main_terms = c("TypeHS", "TypeJC", "OvPck_sc"),
  tv_prefixes = c("TypeHS", "TypeJC", "OvPck_sc"),
  times = c(1, 3, 5, 8, 10)
)

print(mlb_time_effects)

write.csv(
  mlb_time_effects,
  "MLB_time_varying_effects_by_horizon.csv",
  row.names = FALSE
)

ret_time_effects <- make_time_effect_table(
  tv_obj = tv_ret_combined,
  main_terms = c(
    "TypeHS",
    "TypeJC",
    "OvPck_sc",
    "newPOSIF",
    "newPOSLHP",
    "newPOSOF",
    "newPOSRHP"
  ),
  tv_prefixes = c(
    "TypeHS",
    "TypeJC",
    "OvPck_sc",
    "newPOSIF",
    "newPOSLHP",
    "newPOSOF",
    "newPOSRHP"
  ),
  times = c(1, 3, 5, 8, 10)
)

print(ret_time_effects)

write.csv(
  ret_time_effects,
  "Retire_time_varying_effects_by_horizon.csv",
  row.names = FALSE
)

save(
  tv_mlb_combined,
  tv_ret_combined,
  tv_combined_summary,
  mlb_time_effects,
  ret_time_effects,
  file = "time_varying_sensitivity_reduced_final.RData"
)


# -----------------------------
# 7) Final comparison for determining of time varying effects will be implemented in app
# or just a sensitivity analysis test for the paper
# -----------------------------

tv_mlb_combined$fit_base   # static MLB selected model, fit with crr
tv_mlb_combined$fit_tv     # MLB selected model + Type/OvPck x log(time)

tv_ret_combined$fit_base   # static Retire selected model, fit with crr
tv_ret_combined$fit_tv     # Retire selected model + Type/OvPck/newPOS x log(time)

mlb_base_formula <- ~ OvPck_sc*BSp_sc*Type + OvPck_sc*newPOS + Age + Bats + COVID_era
ret_base_formula <- ~ OvPck_sc*BSp_sc*Type + OvPck_sc*BSp_sc + newPOS + Age
mlb_tv_formula <- ~ Type + OvPck_sc
ret_tv_formula <- ~ Type + OvPck_sc + newPOS

## Helpers for static vs time-varying crr prediction comparison

make_X_aligned <- function(formula, newdata, ref_cols) {
  X <- model.matrix(formula, data = newdata)[, -1, drop = FALSE]
  
  missing_cols <- setdiff(ref_cols, colnames(X))
  if (length(missing_cols) > 0) {
    for (cc in missing_cols) {
      X[, cc] <- 0
    }
  }
  
  extra_cols <- setdiff(colnames(X), ref_cols)
  if (length(extra_cols) > 0) {
    X <- X[, setdiff(colnames(X), extra_cols), drop = FALSE]
  }
  
  X <- X[, ref_cols, drop = FALSE]
  return(as.matrix(X))
}

make_tv_X_aligned <- function(tv_formula, newdata, training_data) {
  X_train <- model.matrix(tv_formula, data = training_data)[, -1, drop = FALSE]
  ref_cols <- colnames(X_train)
  
  X_new <- make_X_aligned(tv_formula, newdata, ref_cols)
  colnames(X_new) <- paste0(colnames(X_new), "_x_logtime")
  
  return(X_new)
}

predict_crr_cif <- function(fit, cov1, horizons, cov2 = NULL) {
  
  if (is.null(cov2)) {
    pred <- predict(fit, cov1 = cov1)
  } else {
    pred <- predict(fit, cov1 = cov1, cov2 = cov2)
  }
  
  # predict.crr returns first column as time, then one column per profile
  pred_time <- pred[, 1]
  
  out <- sapply(seq_len(nrow(cov1)), function(i) {
    approx(
      x = pred_time,
      y = pred[, i + 1],
      xout = horizons,
      method = "constant",
      f = 0,
      rule = 2
    )$y
  })
  
  out <- as.data.frame(t(out))
  names(out) <- paste0("t", horizons)
  return(out)
}

# -----------------------------
# Representative player profiles
# -----------------------------

# Use observed quantiles so the profiles are realistic on the scaled variables
pick_q <- quantile(df_tv$OvPck_sc, probs = c(0.05, 0.50, 0.90), na.rm = TRUE)
bsp_q  <- quantile(df_tv$BSp_sc,   probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

profiles <- data.frame(
  profile = c(
    "Early 4Yr hitter",
    "Early HS hitter",
    "Early HS pitcher",
    "Mid 4Yr pitcher",
    "Mid JC hitter",
    "Late 4Yr hitter",
    "Late HS pitcher",
    "Late HS hitter"
  ),
  OvPck_sc = c(
    pick_q[1],
    pick_q[1],
    pick_q[1],
    pick_q[2],
    pick_q[2],
    pick_q[3],
    pick_q[3],
    pick_q[3]
  ),
  BSp_sc = c(
    bsp_q[2],
    bsp_q[2],
    bsp_q[2],
    bsp_q[2],
    bsp_q[2],
    bsp_q[2],
    bsp_q[2],
    bsp_q[2]
  ),
  Type = c(
    "4Yr",
    "HS",
    "HS",
    "4Yr",
    "JC",
    "4Yr",
    "HS",
    "HS"
  ),
  newPOS = c(
    "IF",
    "IF",
    "RHP",
    "RHP",
    "OF",
    "IF",
    "RHP",
    "OF"
  ),
  Age = c(
    21.0,
    18.5,
    18.5,
    21.5,
    20.0,
    22.0,
    18.5,
    18.5
  ),
  Bats = c(
    "R",
    "R",
    "R",
    "R",
    "R",
    "R",
    "R",
    "R"
  ),
  COVID_era = c(
    "Post-COVID",
    "Post-COVID",
    "Post-COVID",
    "Post-COVID",
    "Post-COVID",
    "Post-COVID",
    "Post-COVID",
    "Post-COVID"
  )
)

# Force factor levels to match training data
profiles$Type <- factor(profiles$Type, levels = levels(df_tv$Type))
profiles$newPOS <- factor(profiles$newPOS, levels = levels(df_tv$newPOS))
profiles$Bats <- factor(profiles$Bats, levels = levels(df_tv$Bats))
profiles$COVID_era <- factor(profiles$COVID_era, levels = levels(df_tv$COVID_era))

print(profiles)

# -----------------------------
# MLB model matrices
# -----------------------------

horizons <- c(3, 5, 8, 10)

# Static selected MLB model matrix
X_mlb_train <- model.matrix(mlb_base_formula, data = df_tv)[, -1, drop = FALSE]
X_mlb_profiles <- make_X_aligned(
  formula = mlb_base_formula,
  newdata = profiles,
  ref_cols = colnames(X_mlb_train)
)

# Time-varying MLB covariates
Z_mlb_profiles <- make_tv_X_aligned(
  tv_formula = ~ Type + OvPck_sc,
  newdata = profiles,
  training_data = df_tv
)

# -----------------------------
# MLB static vs TV predictions
# -----------------------------

mlb_static_pred <- predict_crr_cif(
  fit = tv_mlb_combined$fit_base,
  cov1 = X_mlb_profiles,
  horizons = horizons
)

mlb_tv_pred <- predict_crr_cif(
  fit = tv_mlb_combined$fit_tv,
  cov1 = X_mlb_profiles,
  cov2 = Z_mlb_profiles,
  horizons = horizons
)

mlb_compare <- data.frame(
  profile = profiles$profile,
  outcome = "MLB",
  model = "static",
  mlb_static_pred
)

mlb_compare_tv <- data.frame(
  profile = profiles$profile,
  outcome = "MLB",
  model = "time_varying",
  mlb_tv_pred
)

mlb_compare_all <- rbind(mlb_compare, mlb_compare_tv)

print(mlb_compare_all)

mlb_delta <- data.frame(
  profile = profiles$profile,
  outcome = "MLB",
  t3_static  = mlb_static_pred$t3,
  t3_tv      = mlb_tv_pred$t3,
  t3_delta   = mlb_tv_pred$t3 - mlb_static_pred$t3,
  t5_static  = mlb_static_pred$t5,
  t5_tv      = mlb_tv_pred$t5,
  t5_delta   = mlb_tv_pred$t5 - mlb_static_pred$t5,
  t8_static  = mlb_static_pred$t8,
  t8_tv      = mlb_tv_pred$t8,
  t8_delta   = mlb_tv_pred$t8 - mlb_static_pred$t8,
  t10_static = mlb_static_pred$t10,
  t10_tv     = mlb_tv_pred$t10,
  t10_delta  = mlb_tv_pred$t10 - mlb_static_pred$t10
)

print(mlb_delta)

# -----------------------------
# Retire model matrices
# -----------------------------

X_ret_train <- model.matrix(ret_base_formula, data = df_tv)[, -1, drop = FALSE]
X_ret_profiles <- make_X_aligned(
  formula = ret_base_formula,
  newdata = profiles,
  ref_cols = colnames(X_ret_train)
)

Z_ret_profiles <- make_tv_X_aligned(
  tv_formula = ~ Type + OvPck_sc + newPOS,
  newdata = profiles,
  training_data = df_tv
)

# -----------------------------
# Retire static vs TV predictions
# -----------------------------

ret_static_pred <- predict_crr_cif(
  fit = tv_ret_combined$fit_base,
  cov1 = X_ret_profiles,
  horizons = horizons
)

ret_tv_pred <- predict_crr_cif(
  fit = tv_ret_combined$fit_tv,
  cov1 = X_ret_profiles,
  cov2 = Z_ret_profiles,
  horizons = horizons
)

ret_compare <- data.frame(
  profile = profiles$profile,
  outcome = "Retire",
  model = "static",
  ret_static_pred
)

ret_compare_tv <- data.frame(
  profile = profiles$profile,
  outcome = "Retire",
  model = "time_varying",
  ret_tv_pred
)

ret_compare_all <- rbind(ret_compare, ret_compare_tv)

print(ret_compare_all)

ret_delta <- data.frame(
  profile = profiles$profile,
  outcome = "Retire",
  t3_static  = ret_static_pred$t3,
  t3_tv      = ret_tv_pred$t3,
  t3_delta   = ret_tv_pred$t3 - ret_static_pred$t3,
  t5_static  = ret_static_pred$t5,
  t5_tv      = ret_tv_pred$t5,
  t5_delta   = ret_tv_pred$t5 - ret_static_pred$t5,
  t8_static  = ret_static_pred$t8,
  t8_tv      = ret_tv_pred$t8,
  t8_delta   = ret_tv_pred$t8 - ret_static_pred$t8,
  t10_static = ret_static_pred$t10,
  t10_tv     = ret_tv_pred$t10,
  t10_delta  = ret_tv_pred$t10 - ret_static_pred$t10
)

print(ret_delta)

# -----------------------------
# Combined output
# -----------------------------

static_vs_tv_delta <- rbind(mlb_delta, ret_delta)

print(static_vs_tv_delta)

write.csv(
  static_vs_tv_delta,
  "static_vs_time_varying_prediction_comparison.csv",
  row.names = FALSE
)

save(
  profiles,
  mlb_static_pred,
  mlb_tv_pred,
  mlb_delta,
  ret_static_pred,
  ret_tv_pred,
  ret_delta,
  static_vs_tv_delta,
  file = "static_vs_time_varying_prediction_comparison.RData"
)

# -----------------------------
# Max absolute prediction difference
# -----------------------------

delta_cols <- grep("_delta$", names(static_vs_tv_delta), value = TRUE)

max_delta_summary <- data.frame(
  outcome = unique(static_vs_tv_delta$outcome),
  max_abs_delta = sapply(unique(static_vs_tv_delta$outcome), function(out) {
    dat <- static_vs_tv_delta[static_vs_tv_delta$outcome == out, delta_cols]
    max(abs(as.matrix(dat)), na.rm = TRUE)
  })
)

print(max_delta_summary)

write.csv(
  max_delta_summary,
  "static_vs_time_varying_max_delta_summary.csv",
  row.names = FALSE
)

save(
  tv_mlb_combined,
  tv_ret_combined,
  tv_combined_summary,
  mlb_time_effects,
  ret_time_effects,
  static_vs_tv_delta,
  max_delta_summary,
  file = "final_time_varying_model_results.RData"
)

# -----------------------------
# Check combined CIF sanity for representative profiles
# -----------------------------

tv_combined_probs <- merge(
  mlb_delta[, c("profile", "t3_tv", "t5_tv", "t8_tv", "t10_tv")],
  ret_delta[, c("profile", "t3_tv", "t5_tv", "t8_tv", "t10_tv")],
  by = "profile",
  suffixes = c("_MLB", "_Retire")
)

tv_combined_probs$sum_t3  <- tv_combined_probs$t3_tv_MLB  + tv_combined_probs$t3_tv_Retire
tv_combined_probs$sum_t5  <- tv_combined_probs$t5_tv_MLB  + tv_combined_probs$t5_tv_Retire
tv_combined_probs$sum_t8  <- tv_combined_probs$t8_tv_MLB  + tv_combined_probs$t8_tv_Retire
tv_combined_probs$sum_t10 <- tv_combined_probs$t10_tv_MLB + tv_combined_probs$t10_tv_Retire

print(tv_combined_probs[, c("profile", "sum_t3", "sum_t5", "sum_t8", "sum_t10")])

# Flag anything impossible
tv_combined_probs[
  tv_combined_probs$sum_t3 > 1 |
    tv_combined_probs$sum_t5 > 1 |
    tv_combined_probs$sum_t8 > 1 |
    tv_combined_probs$sum_t10 > 1,
]

## Save everything in a CompletedModelSelection.RData
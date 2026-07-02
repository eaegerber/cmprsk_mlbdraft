############################################################
# prediction_helpers.R
#
# Helper functions for DraftSurvival app predictions using
# final time-varying Fine-Gray crr models.
############################################################

# -----------------------------
# 1) Input cleaning / preparation
# -----------------------------

prepare_player_profile <- function(
    model_obj,
    ovpck,
    bonus,
    slot,
    type,
    newpos,
    age,
    bats = "R",
    covid_era = "Post-COVID"
) {
  
  if (is.na(slot) || slot <= 0) {
    stop("Slot value must be greater than 0.")
  }
  
  if (is.na(bonus) || bonus < 0) {
    stop("Bonus must be non-missing and non-negative.")
  }
  
  bsp <- bonus / slot
  
  dat <- data.frame(
    OvPck = ovpck,
    Bonus = bonus,
    Slot = slot,
    BSp = bsp,
    OvPck_sc = (ovpck - model_obj$scaling$OvPck_mean) / model_obj$scaling$OvPck_sd,
    BSp_sc = (bsp - model_obj$scaling$BSp_mean) / model_obj$scaling$BSp_sd,
    Type = type,
    newPOS = newpos,
    Age = age,
    Bats = bats,
    COVID_era = covid_era
  )
  
  dat$Type <- factor(dat$Type, levels = model_obj$factor_levels$Type)
  dat$newPOS <- factor(dat$newPOS, levels = model_obj$factor_levels$newPOS)
  dat$Bats <- factor(dat$Bats, levels = model_obj$factor_levels$Bats)
  dat$COVID_era <- factor(dat$COVID_era, levels = model_obj$factor_levels$COVID_era)
  
  # Catch invalid levels early
  if (any(is.na(dat$Type))) stop("Invalid Type value.")
  if (any(is.na(dat$newPOS))) stop("Invalid newPOS value.")
  if (any(is.na(dat$Bats))) stop("Invalid Bats value.")
  if (any(is.na(dat$COVID_era))) stop("Invalid COVID_era value.")
  
  dat
}


# -----------------------------
# 2) Model matrix helpers
# -----------------------------

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
  as.matrix(X)
}


make_tv_X_aligned <- function(formula, newdata, ref_cols) {
  
  X <- make_X_aligned(
    formula = formula,
    newdata = newdata,
    ref_cols = ref_cols
  )
  
  # Names are not strictly required by predict.crr, but helpful for debugging
  colnames(X) <- paste0(colnames(X), "_x_logtime")
  
  as.matrix(X)
}


# -----------------------------
# 3) Prediction helper
# -----------------------------

predict_crr_cif <- function(fit, cov1, horizons, cov2 = NULL) {
  
  if (is.null(cov2)) {
    pred <- predict(fit, cov1 = cov1)
  } else {
    pred <- predict(fit, cov1 = cov1, cov2 = cov2)
  }
  
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
  out
}


# -----------------------------
# 4) Main app-facing prediction function
# -----------------------------

predict_player_risks <- function(
    model_obj,
    ovpck,
    bonus,
    slot,
    type,
    newpos,
    age,
    bats = "R",
    covid_era = "Post-COVID",
    horizons = model_obj$horizons
) {
  
  profile <- prepare_player_profile(
    model_obj = model_obj,
    ovpck = ovpck,
    bonus = bonus,
    slot = slot,
    type = type,
    newpos = newpos,
    age = age,
    bats = bats,
    covid_era = covid_era
  )
  
  # MLB matrices
  X_mlb <- make_X_aligned(
    formula = model_obj$formulas$mlb_base,
    newdata = profile,
    ref_cols = model_obj$columns$mlb_base
  )
  
  Z_mlb <- make_tv_X_aligned(
    formula = model_obj$formulas$mlb_tv,
    newdata = profile,
    ref_cols = model_obj$columns$mlb_tv
  )
  
  # Retire matrices
  X_ret <- make_X_aligned(
    formula = model_obj$formulas$retire_base,
    newdata = profile,
    ref_cols = model_obj$columns$retire_base
  )
  
  Z_ret <- make_tv_X_aligned(
    formula = model_obj$formulas$retire_tv,
    newdata = profile,
    ref_cols = model_obj$columns$retire_tv
  )
  
  # Predictions
  mlb_pred <- predict_crr_cif(
    fit = model_obj$models$mlb,
    cov1 = X_mlb,
    cov2 = Z_mlb,
    horizons = horizons
  )
  
  ret_pred <- predict_crr_cif(
    fit = model_obj$models$retire,
    cov1 = X_ret,
    cov2 = Z_ret,
    horizons = horizons
  )
  
  out <- data.frame(
    time = horizons,
    MLB = as.numeric(mlb_pred[1, ]),
    Retire = as.numeric(ret_pred[1, ])
  )
  
  out$Unresolved <- 1 - out$MLB - out$Retire
  
  # Safety flags
  out$valid_sum <- out$Unresolved >= -1e-8
  out$MLB <- pmax(pmin(out$MLB, 1), 0)
  out$Retire <- pmax(pmin(out$Retire, 1), 0)
  out$Unresolved <- pmax(pmin(out$Unresolved, 1), 0)
  
  attr(out, "profile") <- profile
  
  out
}
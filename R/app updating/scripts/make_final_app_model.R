############################################################
# make_final_app_model.R
#
# Creates a minimal app-ready RDS object from the completed
# model-selection workspace.
############################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(cmprsk)
})

# -----------------------------
# 1) Paths
# -----------------------------

# Run this script from the repo root.
completed_file <- "CompletedModelSelection.RData"

app_model_dir <- file.path("R/app updating", "model_objects")
app_model_file <- file.path(app_model_dir, "final_app_model.rds")

dir.create(app_model_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(completed_file)) {
  stop("Could not find CompletedModelSelection.RData. Check that you are running from the repo root.")
}

# -----------------------------
# 2) Load completed environment
# -----------------------------

load(completed_file)

# -----------------------------
# 3) Required object checks
# -----------------------------

required_objects <- c(
  "df",
  "df_tv",
  "tv_mlb_combined",
  "tv_ret_combined"
)

missing_objects <- required_objects[!sapply(required_objects, exists)]

if (length(missing_objects) > 0) {
  stop(
    "Missing required objects from CompletedModelSelection.RData: ",
    paste(missing_objects, collapse = ", ")
  )
}

# -----------------------------
# 4) Define final formulas
# -----------------------------

# Static selected model components
mlb_base_formula <- ~ OvPck_sc*BSp_sc*Type + OvPck_sc*newPOS + Age + Bats + COVID_era

ret_base_formula <- ~ OvPck_sc*BSp_sc*Type + OvPck_sc*BSp_sc + newPOS + Age

# Time-varying components
mlb_tv_formula <- ~ Type + OvPck_sc

ret_tv_formula <- ~ Type + OvPck_sc + newPOS

# -----------------------------
# 5) Model matrix column names
# -----------------------------

get_model_cols <- function(formula, data) {
  colnames(model.matrix(formula, data = data)[, -1, drop = FALSE])
}

mlb_base_cols <- get_model_cols(mlb_base_formula, df_tv)
ret_base_cols <- get_model_cols(ret_base_formula, df_tv)

mlb_tv_cols <- get_model_cols(mlb_tv_formula, df_tv)
ret_tv_cols <- get_model_cols(ret_tv_formula, df_tv)

# -----------------------------
# 6) Scaling parameters
# -----------------------------

# These must match the original analysis scaling.
scale_params <- list(
  OvPck_mean = mean(df$OvPck, na.rm = TRUE),
  OvPck_sd   = sd(df$OvPck, na.rm = TRUE),
  BSp_mean   = mean(df$BSp, na.rm = TRUE),
  BSp_sd     = sd(df$BSp, na.rm = TRUE)
)

# Basic checks
if (any(is.na(unlist(scale_params)))) {
  stop("One or more scaling parameters are NA.")
}

if (scale_params$OvPck_sd <= 0 || scale_params$BSp_sd <= 0) {
  stop("One or more scaling SDs are zero or negative.")
}

# -----------------------------
# 7) Factor levels
# -----------------------------

factor_levels <- list(
  Type = levels(df_tv$Type),
  newPOS = levels(df_tv$newPOS),
  Bats = levels(df_tv$Bats),
  COVID_era = levels(df_tv$COVID_era)
)

print(factor_levels)

# -----------------------------
# 8) Training summary metadata
# -----------------------------

training_summary <- list(
  n = nrow(df_tv),
  event_counts = table(df_tv$status),
  event_labels = c(
    "0" = "censored / unresolved",
    "1" = "reached MLB",
    "2" = "retired without MLB"
  ),
  horizons = c(3, 5, 8, 10)
)

# -----------------------------
# 9) Build minimal app object
# -----------------------------

final_app_model <- list(
  version = "final_time_varying_model_2026",
  created_at = as.character(Sys.time()),
  
  models = list(
    mlb = tv_mlb_combined$fit_tv,
    retire = tv_ret_combined$fit_tv
  ),
  
  formulas = list(
    mlb_base = mlb_base_formula,
    retire_base = ret_base_formula,
    mlb_tv = mlb_tv_formula,
    retire_tv = ret_tv_formula
  ),
  
  columns = list(
    mlb_base = mlb_base_cols,
    retire_base = ret_base_cols,
    mlb_tv = mlb_tv_cols,
    retire_tv = ret_tv_cols
  ),
  
  scaling = scale_params,
  
  factor_levels = factor_levels,
  
  horizons = c(3, 5, 8, 10),
  
  training_summary = training_summary,
  
  outcome_codes = list(
    censored = 0,
    mlb = 1,
    retire = 2
  ),
  
  notes = c(
    "Final app model uses time-varying Fine-Gray crr objects.",
    "MLB model includes Type and OvPck_sc varying with log(time).",
    "Retire model includes Type, OvPck_sc, and newPOS varying with log(time).",
    "BSp_sc retained in static model components but not as a time-varying term.",
    "COVID_era should be set to Post-COVID for current/future app predictions unless historical prediction is desired."
  )
)

# -----------------------------
# 10) Sanity checks on object size/content
# -----------------------------

cat("\nFinal app model object size:\n")
print(object.size(final_app_model), units = "MB")

cat("\nTop-level object structure:\n")
str(final_app_model, max.level = 2)

# Make sure no huge data frames accidentally got included
if ("df" %in% names(final_app_model) || "df_tv" %in% names(final_app_model)) {
  stop("Large data frame accidentally included in final_app_model.")
}

# -----------------------------
# 11) Save minimal object
# -----------------------------

saveRDS(
  final_app_model,
  file = app_model_file,
  compress = "xz"
)

cat("\nSaved app model to:\n")
cat(app_model_file, "\n")

cat("\nSaved file size:\n")
print(file.info(app_model_file)$size / 1024^2)
cat("MB\n")

# -----------------------------
# 12) Reload test
# -----------------------------

reloaded <- readRDS(app_model_file)

cat("\nReloaded object names:\n")
print(names(reloaded))

cat("\nReloaded model names:\n")
print(names(reloaded$models))

cat("\nReloaded horizons:\n")
print(reloaded$horizons)
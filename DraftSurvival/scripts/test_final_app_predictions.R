############################################################
# test_final_app_predictions.R
#
# Tests final app prediction object and helper functions
# outside Shiny.
############################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(cmprsk)
})

# Run from repo root
model_path <- "DraftSurvival/model_objects/final_app_model.rds"
helper_path <- "DraftSurvival/R/prediction_helpers.R"

if (!file.exists(model_path)) stop("Could not find final_app_model.rds")
if (!file.exists(helper_path)) stop("Could not find prediction_helpers.R")

final_app_model <- readRDS(model_path)
source(helper_path)

# -----------------------------
# Test profiles
# -----------------------------

test_profiles <- data.frame(
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
  ovpck = c(20, 20, 20, 300, 300, 900, 900, 900),
  bonus = c(3.0, 3.0, 3.0, 0.5, 0.5, 0.15, 0.15, 0.15),
  slot = c(3.0, 3.0, 3.0, 0.5, 0.5, 0.15, 0.15, 0.15),
  type = c("4Yr", "HS", "HS", "4Yr", "JC", "4Yr", "HS", "HS"),
  newpos = c("IF", "IF", "RHP", "RHP", "OF", "IF", "RHP", "OF"),
  age = c(21.0, 18.5, 18.5, 21.5, 20.0, 22.0, 18.5, 18.5),
  bats = c("R", "R", "R", "R", "R", "R", "R", "R")
)

# -----------------------------
# Run predictions
# -----------------------------

all_preds <- lapply(seq_len(nrow(test_profiles)), function(i) {
  
  p <- test_profiles[i, ]
  
  pred <- predict_player_risks(
    model_obj = final_app_model,
    ovpck = p$ovpck,
    bonus = p$bonus,
    slot = p$slot,
    type = p$type,
    newpos = p$newpos,
    age = p$age,
    bats = p$bats,
    covid_era = "Post-COVID"
  )
  
  pred$profile <- p$profile
  
  pred
})

all_preds <- do.call(rbind, all_preds)

# Reorder
all_preds <- all_preds[, c("profile", "time", "MLB", "Retire", "Unresolved", "valid_sum")]

print(all_preds)

# -----------------------------
# Checks
# -----------------------------

# 1) No impossible sums
invalid_sum <- all_preds[!all_preds$valid_sum, ]
print(invalid_sum)

# 2) Probabilities in [0, 1]
prob_cols <- c("MLB", "Retire", "Unresolved")
range_check <- sapply(all_preds[, prob_cols], range, na.rm = TRUE)
print(range_check)

# 3) Monotonicity by profile
check_monotone_by_profile <- function(dat, col) {
  all(diff(dat[[col]]) >= -1e-8)
}

monotone_checks <- do.call(
  rbind,
  lapply(split(all_preds, all_preds$profile), function(dat) {
    data.frame(
      profile = unique(dat$profile),
      MLB_monotone = check_monotone_by_profile(dat, "MLB"),
      Retire_monotone = check_monotone_by_profile(dat, "Retire")
    )
  })
)

print(monotone_checks)

# 4) Save test output
out_dir <- "DraftSurvival/test_outputs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

write.csv(
  all_preds,
  file.path(out_dir, "test_final_app_predictions.csv"),
  row.names = FALSE
)

write.csv(
  monotone_checks,
  file.path(out_dir, "test_final_app_prediction_monotonicity.csv"),
  row.names = FALSE
)

cat("\nPrediction helper test complete.\n")
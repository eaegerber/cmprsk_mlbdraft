############################################################
# test_compare_players.R
#
# Smoke test for the Compare Players data artifact and the
# existing production prediction helper.
#
# Run from repository root:
#   Rscript DraftSurvival/scripts/test_compare_players.R
############################################################

library(cmprsk)

model_path <- file.path(
  "DraftSurvival",
  "model_objects",
  "final_app_model.rds"
)

lookup_path <- file.path(
  "DraftSurvival",
  "model_objects",
  "player_lookup.rds"
)

helper_path <- file.path(
  "DraftSurvival",
  "R",
  "prediction_helpers.R"
)

for (path in c(model_path, lookup_path, helper_path)) {
  if (!file.exists(path)) {
    stop(
      "Missing required file: ", path,
      ". Run this script from the repository root."
    )
  }
}

model_obj <- readRDS(model_path)
lookup_bundle <- readRDS(lookup_path)
source(helper_path)

players <- lookup_bundle$players

stopifnot(
  is.data.frame(players),
  nrow(players) > 0,
  !anyDuplicated(players$player_key)
)

find_player <- function(name, year) {
  row <- players[
    tolower(players$Name) == tolower(name) &
      players$Year == year,
    ,
    drop = FALSE
  ]

  if (nrow(row) == 0) {
    stop("Could not find ", name, " (", year, ") in player_lookup.rds")
  }

  row[1, , drop = FALSE]
}

predict_row <- function(row) {
  predict_player_risks(
    model_obj = model_obj,
    ovpck = row$OvPck,
    bonus = row$Bonus,
    slot = row$Slot,
    type = row$Type,
    newpos = row$newPOS,
    age = row$Age,
    bats = row$Bats,
    covid_era = "Post-COVID",
    horizons = 1:10
  )
}

player_a <- find_player("Alex Bregman", 2015)
player_b <- find_player("Kris Bryant", 2013)

pred_a <- predict_row(player_a)
pred_b <- predict_row(player_b)

for (pred in list(pred_a, pred_b)) {
  stopifnot(
    all(pred$valid_sum),
    all(pred$MLB >= 0 & pred$MLB <= 1),
    all(pred$Retire >= 0 & pred$Retire <= 1),
    all(pred$Unresolved >= 0 & pred$Unresolved <= 1),
    all(diff(pred$MLB) >= -1e-10),
    all(diff(pred$Retire) >= -1e-10)
  )
}

row6_a <- pred_a[pred_a$time == 6, , drop = FALSE]
row6_b <- pred_b[pred_b$time == 6, , drop = FALSE]

cat("\nCompare Players smoke test PASSED.\n\n")
cat(
  player_a$display_label,
  "\n  6-year MLB:",
  round(100 * row6_a$MLB, 1), "%",
  "\n  6-year Retire:",
  round(100 * row6_a$Retire, 1), "%",
  "\n  6-year Still Playing:",
  round(100 * row6_a$Unresolved, 1), "%\n\n"
)

cat(
  player_b$display_label,
  "\n  6-year MLB:",
  round(100 * row6_b$MLB, 1), "%",
  "\n  6-year Retire:",
  round(100 * row6_b$Retire, 1), "%",
  "\n  6-year Still Playing:",
  round(100 * row6_b$Unresolved, 1), "%\n"
)

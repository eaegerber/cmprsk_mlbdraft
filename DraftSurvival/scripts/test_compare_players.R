############################################################
# test_compare_players.R
#
# Smoke/regression checks for the Compare Players lookup,
# prediction behavior, draft-era handling, and UI series setup.
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

compare_path <- file.path(
  "DraftSurvival",
  "R",
  "compare_players.R"
)

for (path in c(model_path, lookup_path, helper_path, compare_path)) {
  if (!file.exists(path)) {
    stop(
      "Missing required file: ", path,
      ". Run this script from the repository root."
    )
  }
}

if (!requireNamespace("plotly", quietly = TRUE)) {
  stop(
    "The Compare Players feature now requires the plotly package. ",
    "Install it with install.packages('plotly')."
  )
}

model_obj <- readRDS(model_path)
lookup_bundle <- readRDS(lookup_path)
source(helper_path)
source(compare_path)

players <- lookup_bundle$players

required_cols <- c(
  "player_key", "display_label", "person_id", "Name", "Year", "Tm",
  "OvPck", "Bonus", "Slot", "Type", "newPOS", "Age", "Bats", "COVID_era"
)

stopifnot(
  is.data.frame(players),
  nrow(players) > 0,
  all(required_cols %in% names(players)),
  !anyDuplicated(players$player_key)
)

# The four graph series must remain independently toggleable.
series_choices <- cp_series_choices("Player A", "Player B")
stopifnot(
  length(series_choices) == 4,
  identical(
    unname(series_choices),
    c("a_mlb", "a_retire", "b_mlb", "b_retire")
  )
)

# If multiple model-eligible draft events exist for a person, each event must
# remain distinguishable by the event key. If none exist in the current signed
# cohort, the UI still supports the case and earlier unsigned events can be
# recreated manually.
dup_person_ids <- unique(
  players$person_id[
    duplicated(players$person_id) |
      duplicated(players$person_id, fromLast = TRUE)
  ]
)

if (length(dup_person_ids) > 0) {
  for (pid in dup_person_ids) {
    grp <- players[players$person_id == pid, , drop = FALSE]
    stopifnot(
      nrow(grp) > 1,
      length(unique(grp$player_key)) == nrow(grp),
      length(unique(paste(grp$Year, grp$OvPck, sep = "__"))) == nrow(grp)
    )
  }
}

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

predict_row <- function(row, covid_era = "Post-COVID") {
  predict_player_risks(
    model_obj = model_obj,
    ovpck = row$OvPck,
    bonus = row$Bonus,
    slot = row$Slot,
    type = row$Type,
    newpos = row$newPOS,
    age = row$Age,
    bats = row$Bats,
    covid_era = covid_era,
    horizons = 1:10
  )
}

check_prediction_shape <- function(pred) {
  stopifnot(
    nrow(pred) == 10,
    identical(as.integer(pred$time), 1:10),
    all(is.finite(pred$MLB)),
    all(is.finite(pred$Retire)),
    all(is.finite(pred$Unresolved)),
    all(pred$MLB >= 0 & pred$MLB <= 1),
    all(pred$Retire >= 0 & pred$Retire <= 1),
    all(pred$Unresolved >= 0 & pred$Unresolved <= 1),
    all(diff(pred$MLB) >= -1e-10),
    all(diff(pred$Retire) >= -1e-10)
  )
}

player_a <- find_player("Alex Bregman", 2015)
player_b <- find_player("Kris Bryant", 2013)

# Post-COVID remains the stable regression baseline used by the existing
# Predict tab. These two profiles should remain valid across years 1-10.
pred_a_post <- predict_row(player_a, "Post-COVID")
pred_b_post <- predict_row(player_b, "Post-COVID")

for (pred in list(pred_a_post, pred_b_post)) {
  check_prediction_shape(pred)
  stopifnot(all(pred$valid_sum))
}

# Manual profiles can now explicitly choose either era. The current fitted
# model uses COVID era for reaching MLB, while the retirement prediction is
# unchanged by that setting. Some Pre-COVID profiles may have valid_sum ==
# FALSE at late horizons; the UI now warns rather than rejecting them.
pred_a_pre <- predict_row(player_a, "Pre-COVID")
check_prediction_shape(pred_a_pre)

stopifnot(
  any(abs(pred_a_pre$MLB - pred_a_post$MLB) > 1e-8),
  max(abs(pred_a_pre$Retire - pred_a_post$Retire)) < 1e-10
)

row6_a <- pred_a_post[pred_a_post$time == 6, , drop = FALSE]
row6_b <- pred_b_post[pred_b_post$time == 6, , drop = FALSE]

cat("\nCompare Players smoke test PASSED.\n\n")
cat("Four independently toggleable graph series: PASS\n")
cat("Manual Pre-/Post-COVID prediction behavior: PASS\n")
cat(
  "Repeated model-eligible person IDs found:",
  length(dup_person_ids),
  "(event keys remain unique)\n\n"
)

cat(
  player_a$display_label,
  "\n  Post-COVID 6-year MLB:",
  round(100 * row6_a$MLB, 1), "%",
  "\n  Post-COVID 6-year Retire:",
  round(100 * row6_a$Retire, 1), "%",
  "\n  Post-COVID 6-year Still Playing:",
  round(100 * row6_a$Unresolved, 1), "%\n\n"
)

cat(
  player_b$display_label,
  "\n  Post-COVID 6-year MLB:",
  round(100 * row6_b$MLB, 1), "%",
  "\n  Post-COVID 6-year Retire:",
  round(100 * row6_b$Retire, 1), "%",
  "\n  Post-COVID 6-year Still Playing:",
  round(100 * row6_b$Unresolved, 1), "%\n"
)

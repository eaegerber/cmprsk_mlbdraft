############################################################
# make_player_lookup.R
#
# Builds a lightweight app-ready historical player lookup
# from the SAME cleaned_df2.csv population used by the final
# model-selection pipeline.
#
# Run this script from the REPOSITORY ROOT:
#   Rscript DraftSurvival/scripts/make_player_lookup.R
############################################################

input_path <- file.path("R", "cleaned_df2.csv")
output_dir <- file.path("DraftSurvival", "model_objects")
output_path <- file.path(output_dir, "player_lookup.rds")

if (!file.exists(input_path)) {
  stop(
    "Could not find ", input_path, ". ",
    "Run this script from the cmprsk_mlbdraft repository root."
  )
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

raw <- read.csv(
  input_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_cols <- c(
  "Year", "Rnd", "OvPck", "Tm", "Bonus", "Slot", "Name",
  "Pos", "Type", "COVID_era", "Bats", "Throws", "Age",
  "newPOS", "BSp", "BmS", "times", "status", "Signed",
  "person_id", "has_later_draft"
)

missing_cols <- setdiff(required_cols, names(raw))
if (length(missing_cols) > 0) {
  stop(
    "cleaned_df2.csv is missing required columns: ",
    paste(missing_cols, collapse = ", ")
  )
}

# Mirror the final-model data preparation:
# 1) signed records only
# 2) complete cases
# 3) valid Type / Bats / Throws
# 4) convert generic P to handed pitcher group
signed_flag <- toupper(trimws(as.character(raw$Signed))) == "TRUE"
signed_flag[is.na(signed_flag)] <- FALSE

df <- raw[signed_flag, , drop = FALSE]
df <- df[complete.cases(df), , drop = FALSE]

df <- df[df$Type %in% c("4Yr", "HS", "JC"), , drop = FALSE]
df <- df[df$Bats %in% c("B", "L", "R"), , drop = FALSE]
df <- df[df$Throws %in% c("L", "R"), , drop = FALSE]

df$Pos[df$Pos == "P" & df$Throws == "L"] <- "LHP"
df$Pos[df$Pos == "P" & df$Throws == "R"] <- "RHP"
df$newPOS[df$newPOS == "P" & df$Throws == "L"] <- "LHP"
df$newPOS[df$newPOS == "P" & df$Throws == "R"] <- "RHP"

# Final app-facing validity checks. These should not remove any rows
# from the current 2026 final-model cohort; they make the lookup's
# assumptions explicit.
df <- df[
  df$newPOS %in% c("C", "IF", "OF", "LHP", "RHP") &
    df$Slot > 0 &
    df$Bonus >= 0 &
    df$OvPck >= 1 &
    df$Age >= 16 &
    df$Age <= 25,
  ,
  drop = FALSE
]

# Current final modeling/reporting cohort is 9,099 rows. Warn rather
# than stop so a future legitimate data refresh can still rebuild.
if (nrow(df) != 9099L) {
  warning(
    "The lookup contains ", nrow(df),
    " rows; the current 2026 final-model cohort is expected to contain 9,099. ",
    "If the research data have intentionally changed, verify the new count before deployment."
  )
}

# Clean display fields.
df$Name <- trimws(as.character(df$Name))
df$Tm <- trimws(as.character(df$Tm))
df$Year <- as.integer(df$Year)
df$Rnd <- as.integer(df$Rnd)
df$OvPck <- as.integer(df$OvPck)
df$Age <- as.numeric(df$Age)
df$Bonus <- as.numeric(df$Bonus)
df$Slot <- as.numeric(df$Slot)
df$person_id <- as.numeric(df$person_id)

person_id_text <- sprintf("%.0f", df$person_id)

df$player_key <- paste(
  person_id_text,
  df$Year,
  df$OvPck,
  sep = "__"
)

df$display_label <- paste0(
  df$Name,
  " - ",
  df$Year,
  " ",
  df$Tm,
  " - Pick #",
  df$OvPck
)

if (anyDuplicated(df$player_key)) {
  dupes <- unique(df$player_key[duplicated(df$player_key)])
  stop(
    "player_key is not unique. Example duplicate(s): ",
    paste(head(dupes, 10), collapse = ", ")
  )
}

players <- df[
  ,
  c(
    "player_key",
    "display_label",
    "person_id",
    "Name",
    "Year",
    "Rnd",
    "OvPck",
    "Tm",
    "Bonus",
    "Slot",
    "Type",
    "newPOS",
    "Age",
    "Bats",
    "COVID_era"
  ),
  drop = FALSE
]

players <- players[
  order(tolower(players$Name), players$Year, players$OvPck),
  ,
  drop = FALSE
]
rownames(players) <- NULL

make_range <- function(x) {
  x <- as.numeric(x)
  c(min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE))
}

support <- list(
  OvPck = make_range(df$OvPck),
  Bonus = make_range(df$Bonus),
  Slot = make_range(df$Slot),
  BSp = make_range(df$BSp),
  Age = make_range(df$Age)
)

lookup_bundle <- list(
  version = "compare_player_lookup_v1",
  source = "R/cleaned_df2.csv",
  model_population = "Signed complete-case records used by the final model pipeline",
  n_players = nrow(players),
  players = players,
  support = support
)

saveRDS(
  lookup_bundle,
  output_path,
  compress = "xz"
)

cat("\nCreated:", output_path, "\n")
cat("Historical draft profiles:", nrow(players), "\n")
cat(
  "Years:",
  min(players$Year),
  "to",
  max(players$Year),
  "\n"
)
cat(
  "Overall pick range:",
  support$OvPck["min"],
  "to",
  support$OvPck["max"],
  "\n"
)
cat(
  "Bonus range:",
  support$Bonus["min"],
  "to",
  support$Bonus["max"],
  "million\n"
)
cat(
  "Slot range:",
  support$Slot["min"],
  "to",
  support$Slot["max"],
  "million\n"
)
cat(
  "Bonus/slot range:",
  support$BSp["min"],
  "to",
  support$BSp["max"],
  "\n"
)

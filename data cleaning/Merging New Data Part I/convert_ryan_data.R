# convert_ryan_data.R
# Converts Ryan's scraped draft data (2010-2024) into my original cleaned_df.csv schema
# and validates overlap years 2012-2016 vs cleaned_df.csv.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(tidyr)
  library(readr)
})

# ----------------------------
# Helpers
# ----------------------------

normalize_name <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "UTF-8", sub = "")
  x <- gsub("\u00A0", " ", x, fixed = TRUE)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

normalize_bats <- function(x) {
  x2 <- tolower(trimws(iconv(as.character(x), from = "", to = "UTF-8", sub = "")))
  ifelse(x2 %in% c("r","right"), "R",
  ifelse(x2 %in% c("l","left"), "L",
  ifelse(x2 %in% c("s","switch","b","both"), "B", NA)))
}

normalize_throws <- function(x) {
  x2 <- tolower(trimws(iconv(as.character(x), from = "", to = "UTF-8", sub = "")))
  ifelse(x2 %in% c("r","right"), "R",
  ifelse(x2 %in% c("l","left"), "L", NA))
}

parse_bonus_millions <- function(x) {
  # Input examples: "$1,234,567", "0", "", NA
  x_chr <- as.character(x)
  x_chr <- str_replace_all(x_chr, "\\$", "")
  x_chr <- str_replace_all(x_chr, ",", "")
  x_chr <- str_trim(x_chr)

  suppressWarnings({
    val <- as.numeric(ifelse(x_chr == "" | is.na(x_chr), NA, x_chr))
  })

  # Convention: missing bonus -> 0.0 (matches your earlier cleaned_df style)
  val <- ifelse(is.na(val), 0, val)
  val / 1e6
}

map_type <- function(x) {
  # Student: "High School", "4YR", "Junior College", etc.
  x2 <- str_to_lower(str_squish(as.character(x)))
  case_when(
    str_detect(x2, "hs") | str_detect(x2, "high") ~ "HS",
    str_detect(x2, "junior") | str_detect(x2, "jc") ~ "JC",
    str_detect(x2, "4") | str_detect(x2, "college") | str_detect(x2, "univ") ~ "4Yr",
    TRUE ~ NA_character_
  )
}

extract_year_from_date <- function(x) {
  # x might be "2015-06-08" or blank
  if (inherits(x, "Date")) return(year(x))
  x_chr <- str_trim(as.character(x))
  x_chr[x_chr == ""] <- NA_character_
  # try ymd
  d <- suppressWarnings(ymd(x_chr))
  y <- suppressWarnings(as.integer(year(d)))
  # if ymd failed but it’s already a year, fall back
  y[is.na(y)] <- suppressWarnings(as.integer(x_chr[is.na(y)]))
  y
}

compute_time_status_year_based <- function(df, asof_year) {
  debut_y <- extract_year_from_date(df$`MLB Debut`)
  draft_y <- as.integer(df$Draft)
  last_y  <- suppressWarnings(as.integer(df$`Last Year`))
  active  <- suppressWarnings(as.integer(df$Active))

  status <- case_when(
    !is.na(debut_y) ~ 1L,
    is.na(debut_y) & !is.na(active) & active == 1L ~ 0L,
    TRUE ~ 2L
  )

  times <- case_when(
    status == 1L ~ pmax(0L, debut_y - draft_y + 1),
    status == 0L ~ pmax(0L, asof_year - draft_y + 1),
    status == 2L ~ pmax(0L, last_y - draft_y + 1)
  )

  list(times = as.integer(times), status = as.integer(status))
}

agree_rate <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  if (sum(ok) == 0) return(NA_real_)
  mean(a[ok] == b[ok])
}

within_tol <- function(a, b, tol = 0) {
  ok <- !is.na(a) & !is.na(b)
  if (sum(ok) == 0) return(NA_real_)
  mean(abs(a[ok] - b[ok]) <= tol)
}

# ----------------------------
# Inputs
# ----------------------------

student_path <- "./data cleaning/mlb_draft_picks.csv"
cleaned_path <- "./data cleaning/cleaned_df.csv"

student <- read_csv(student_path, show_col_types = FALSE)
cleaned <- read_csv(cleaned_path, show_col_types = FALSE)

# If you have stringi installed (it usually comes with stringr)
if (!requireNamespace("stringi", quietly = TRUE)) install.packages("stringi")
library(stringi)

# Convert *all* character columns to valid UTF-8, substituting bad bytes
student <- student %>%
  mutate(across(where(is.character), ~ stringi::stri_enc_toutf8(.x, is_unknown_8bit = TRUE)))

cleaned <- cleaned %>%
  mutate(across(where(is.character), ~ stringi::stri_enc_toutf8(.x, is_unknown_8bit = TRUE)))

names(student) <- names(student) %>%
  str_replace_all("\\.", " ") %>%
  str_squish()

required_cols <- c(
  "Draft","Round","Pick","Name","Signing Bonus","School Type",
  "MLB Debut","Active","Last Year","Age at Draft","Bats","Throws"
)

missing_req <- setdiff(required_cols, names(student))
if (length(missing_req) > 0) {
  stop("Student file missing required columns: ", paste(missing_req, collapse = ", "))
}

# ----------------------------
# Step 1B: Convert student -> cleaned_df schema (as-of 2024)
# ----------------------------

student2 <- student %>%
  mutate(
    Draft  = as.integer(Draft),
    Round  = as.integer(Round),
    Pick   = as.integer(Pick),
    Name   = normalize_name(Name),
    Type   = map_type(`School Type`),
    Bats   = normalize_bats(Bats),
    Throws = normalize_throws(Throws),
    Age    = suppressWarnings(as.numeric(`Age at Draft`)),
    Bonus  = parse_bonus_millions(`Signing Bonus`)
  ) %>%
  arrange(Draft, Round, Pick, Name) %>%
  group_by(Draft) %>%
  mutate(OvPck = row_number()) %>%
  ungroup()

ts <- compute_time_status_year_based(student2, asof_year = 2024)

student_asof2024 <- student2 %>%
  transmute(
    Year   = Draft,
    Rnd    = Round,
    OvPck  = OvPck,
    Tm     = NA_character_,   # filled later
    Bonus  = Bonus,           # millions
    Slot   = NA_real_,        # filled later
    Name   = Name,
    Pos    = NA_character_,   # intentionally blank
    Type   = Type,
    Bats   = Bats,
    Throws = Throws,
    Age    = Age,
    Pitch  = NA_character_,   # intentionally blank
    newPOS = NA_character_,   # intentionally blank
    BSp    = NA_real_,        # filled later
    BmS    = NA_real_,        # filled later
    times  = ts$times,
    status = ts$status
  ) %>%
  select(Year, Rnd, OvPck, Tm, Bonus, Slot, Name, Pos, Type, Bats, Throws,
         Age, Pitch, newPOS, BSp, BmS, times, status)

#write_csv(student_asof2024, "./data cleaning/converted_schema_asof2024.csv", na = "")
## This has been manually edited to account for issues identified below; the edited version is what we will read in later

# ----------------------------
# Step 1C: Validate *covariates* only (2012-2016 overlap)
# ----------------------------

overlap_years <- 2012:2016

cleaned2 <- cleaned %>%
  mutate(
    Year  = as.integer(Year),
    Rnd   = as.integer(Rnd),
    OvPck = as.integer(OvPck),
    Name_norm = normalize_name(Name),
    Age = suppressWarnings(as.numeric(Age)),
    Bonus = suppressWarnings(as.numeric(Bonus))
  )

student_val <- student_asof2024 %>%
  mutate(
    Year  = as.integer(Year),
    Rnd   = as.integer(Rnd),
    OvPck = as.integer(OvPck),
    Name_norm = normalize_name(Name),
    Age = suppressWarnings(as.numeric(Age)),
    Bonus = suppressWarnings(as.numeric(Bonus))
  )

joined <- cleaned2 %>%
  filter(Year %in% overlap_years) %>%
  inner_join(
    student_val %>% filter(Year %in% overlap_years),
    by = c("Year","Rnd","OvPck"),
    suffix = c(".cleaned",".student")
  )

metrics <- tibble(
  matched_rows = nrow(joined),
  bonus_match_exact = within_tol(joined$Bonus.cleaned, joined$Bonus.student, tol = 1e-6),
  age_match_exact   = within_tol(joined$Age.cleaned,   joined$Age.student,   tol = 1e-6),
  type_match        = agree_rate(joined$Type.cleaned,  joined$Type.student),
  bats_match        = agree_rate(joined$Bats.cleaned,  joined$Bats.student),
  throws_match      = agree_rate(joined$Throws.cleaned,joined$Throws.student),
  name_match        = agree_rate(joined$Name_norm.cleaned, joined$Name_norm.student)
)

as.data.frame(metrics)

# Save covariate mismatches for debugging (optional but useful)
cov_mismatch <- joined %>%
  filter(
    (abs(Bonus.cleaned - Bonus.student) > 1e-6) |
    (abs(Age.cleaned   - Age.student)   > 1e-6) |
    (Type.cleaned      != Type.student) |
    (Bats.cleaned      != Bats.student) |
    (Throws.cleaned    != Throws.student)
  ) %>%
  transmute(
    Year, Rnd, OvPck,
    Name.cleaned, Name.student,
    Bonus.cleaned, Bonus.student,
    Age.cleaned, Age.student,
    Type.cleaned, Type.student,
    Bats.cleaned, Bats.student,
    Throws.cleaned, Throws.student
  )

nrow(cov_mismatch)
head(cov_mismatch)

write_csv(cov_mismatch, "./data cleaning/validation_covariate_mismatches_2012_2016.csv", na = "")

age_mismatch <- cov_mismatch %>%
  filter(abs(Age.cleaned - Age.student) > 1e-6) %>%
  arrange(desc(abs(Age.cleaned - Age.student)))

print(age_mismatch, n = 50)


# Read in manually edited version of converted_schema_asof2024.csv (with fixes for identified issues)
#student_asof2024_fixed <- read_csv("./data cleaning/converted_schema_asof2024.csv", show_col_types = FALSE)
#head(student_asof2024_fixed)
# Fixed it further based on below; don't run above

# Check if time and status match

# Join on the key
joined_outcomes <- cleaned %>%
  mutate(
    Year = as.integer(Year), Rnd = as.integer(Rnd), OvPck = as.integer(OvPck),
    status = as.integer(status), times = as.integer(times)
  ) %>%
  inner_join(
    student_asof2024_fixed %>%
      mutate(
        Year = as.integer(Year), Rnd = as.integer(Rnd), OvPck = as.integer(OvPck),
        status = as.integer(status), times = as.integer(times)
      ),
    by = c("Year","Rnd","OvPck"),
    suffix = c(".old", ".new")
  )

# Focus: players already "observed" in your old file
observed_old <- joined_outcomes %>%
  filter(status.old %in% c(1,2))

# Summaries
summary_tbl <- observed_old %>%
  summarise(
    n = n(),
    status_match = mean(status.old == status.new, na.rm = TRUE),
    times_match  = mean(times.old  == times.new,  na.rm = TRUE),
    n_status_mismatch = sum(status.old != status.new, na.rm = TRUE),
    n_times_mismatch  = sum(times.old  != times.new,  na.rm = TRUE)
  )

print(summary_tbl)

# Inspect the mismatches (this is the important output)
status_mismatches_observed <- observed_old %>%
  filter(status.old != status.new) %>%
  select(Year, Rnd, OvPck, Name.old, Name.new, status.old, status.new, times.old, times.new)

times_mismatches_observed <- observed_old %>%
  filter(status.old == status.new, status.old == 1, times.old != times.new) %>%  # restrict to MLB where times should be stable
  select(Year, Rnd, OvPck, Name.old, Name.new, status.old, status.new, times.old, times.new)

write_csv(status_mismatches_observed, "./data cleaning/observed_status_mismatches.csv")
write_csv(times_mismatches_observed,  "./data cleaning/observed_times_mismatches_status1.csv")

# Should be mostly good now
# Have completeley corrected manually; load it in now
# Read in manually edited version of converted_schema_asof2024.csv (with fixes for identified issues)
student_asof2024_fixed <- read_csv("./data cleaning/converted_schema_asof2024.csv", show_col_types = FALSE)
head(student_asof2024_fixed)

# Let's go ahead and add a column for pre-COVID vs post-COVID draft, since that will be a key covariate in our analysis and we want to make sure it's consistent
student_asof2024_fixed <- student_asof2024_fixed %>%
  mutate(COVID_era = ifelse(Year >= 2020, "Post-COVID", "Pre-COVID"))

tail(as.data.frame(student_asof2024_fixed))
write_csv(student_asof2024_fixed, "./data cleaning/final_converted_schema.csv", na = "")

# ----------------------------
# Optional: outcome changes table (old cleaned vs as-of-2024 student)
# ----------------------------

outcome_changes <- joined %>%
  transmute(
    Year, Rnd, OvPck,
    Name.cleaned, Name.student,
    status.cleaned, times.cleaned,
    status.student, times.student,
    changed_status = (status.cleaned != status.student),
    changed_times  = (times.cleaned  != times.student)
  ) %>%
  filter(changed_status | changed_times) %>%
  arrange(Year, Rnd, OvPck)

write_csv(outcome_changes, "./data cleaning/outcome_changes_2012_2016_cleaned_vs_asof2024.csv", na = "")

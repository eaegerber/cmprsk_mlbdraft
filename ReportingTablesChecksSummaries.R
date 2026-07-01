## Final Checks and Public Facing Tables

# -----------------------------
# 0) Setup
# -----------------------------

rm(list = ls())

load("CompletedModelSelection.RData")

out_dir <- "final_model_reporting_outputs"
if (!dir.exists(out_dir)) dir.create(out_dir)

# Optional but helpful
suppressPackageStartupMessages({
  library(data.table)
})

# Small helper
write_out <- function(x, filename) {
  write.csv(x, file.path(out_dir, filename), row.names = FALSE)
}

round_num <- function(x, digits = 3) {
  ifelse(is.numeric(x), round(x, digits), x)
}

# -----------------------------
# 1) Final sample / event summary
# -----------------------------

# Prefer df_tv/df_fgr for final analysis frame
df_analysis <- if (exists("df_tv")) {
  df_tv
} else if (exists("df_fgr")) {
  df_fgr
} else {
  stop("Could not find df_tv or df_fgr in CompletedModelSelection.RData")
}

# Ensure event labels
if (!("status_fg" %in% names(df_analysis))) {
  df_analysis$status_fg <- factor(
    df_analysis$status,
    levels = c(0, 1, 2),
    labels = c("censored", "MLB", "Retire")
  )
}

event_summary <- as.data.frame(table(df_analysis$status_fg))
names(event_summary) <- c("event_status", "n")
event_summary$percent <- round(100 * event_summary$n / sum(event_summary$n), 1)

final_sample_summary <- data.frame(
  metric = c(
    "Final analysis N",
    "Reached MLB",
    "Retired without MLB",
    "Censored / still unresolved"
  ),
  value = c(
    nrow(df_analysis),
    sum(df_analysis$status == 1, na.rm = TRUE),
    sum(df_analysis$status == 2, na.rm = TRUE),
    sum(df_analysis$status == 0, na.rm = TRUE)
  )
)

print(final_sample_summary)
print(event_summary)

write_out(final_sample_summary, "Table1_final_sample_summary.csv")
write_out(event_summary, "Table1_event_status_counts.csv")

# -----------------------------
# 2) Categorical summaries
# -----------------------------

type_summary <- as.data.frame(table(df_analysis$Type, useNA = "ifany"))
names(type_summary) <- c("Type", "n")
type_summary$percent <- round(100 * type_summary$n / sum(type_summary$n), 1)

newpos_summary <- as.data.frame(table(df_analysis$newPOS, useNA = "ifany"))
names(newpos_summary) <- c("newPOS", "n")
newpos_summary$percent <- round(100 * newpos_summary$n / sum(newpos_summary$n), 1)

print(type_summary)
print(newpos_summary)

write_out(type_summary, "Table1_type_distribution.csv")
write_out(newpos_summary, "Table1_newPOS_distribution.csv")

# -----------------------------
# 3) Continuous summaries
# -----------------------------

df_for_continuous <- if (exists("df")) df else df_analysis

continuous_vars <- c("Age", "OvPck", "BSp", "BmS", "Bonus", "Slot", "times")
continuous_vars <- continuous_vars[continuous_vars %in% names(df_for_continuous)]

summ_cont <- function(x) {
  c(
    n = sum(!is.na(x)),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    q25 = quantile(x, 0.25, na.rm = TRUE),
    q75 = quantile(x, 0.75, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )
}

continuous_summary <- do.call(
  rbind,
  lapply(continuous_vars, function(v) {
    out <- as.data.frame(t(summ_cont(df_for_continuous[[v]])))
    out$variable <- v
    out
  })
)

continuous_summary <- continuous_summary[, c("variable", "n", "mean", "sd", "median", "q25.25%", "q75.75%", "min", "max")]
continuous_summary[, -1] <- lapply(continuous_summary[, -1], function(x) round(as.numeric(x), 3))

print(continuous_summary)

write_out(continuous_summary, "Table1_continuous_variable_summary.csv")

# -----------------------------
# 4) Event counts by Type and newPOS
# -----------------------------

event_by_type <- as.data.frame.matrix(table(df_analysis$Type, df_analysis$status_fg))
event_by_type$Type <- rownames(event_by_type)
event_by_type <- event_by_type[, c("Type", setdiff(names(event_by_type), "Type"))]

event_by_newpos <- as.data.frame.matrix(table(df_analysis$newPOS, df_analysis$status_fg))
event_by_newpos$newPOS <- rownames(event_by_newpos)
event_by_newpos <- event_by_newpos[, c("newPOS", setdiff(names(event_by_newpos), "newPOS"))]

print(event_by_type)
print(event_by_newpos)

write_out(event_by_type, "Table1_event_counts_by_Type.csv")
write_out(event_by_newpos, "Table1_event_counts_by_newPOS.csv")

# -----------------------------
# 5) Static selected model validation: B = 100 bootstrap CV
# -----------------------------

get_score_summary <- function(score_obj, outcome_label) {
  s <- summary(score_obj)
  out <- as.data.frame(s$score)
  out$outcome <- outcome_label
  out <- out[, c("outcome", setdiff(names(out), "outcome"))]
  out
}

get_score_contrasts <- function(score_obj, outcome_label) {
  s <- summary(score_obj)
  out <- as.data.frame(s$contrasts)
  out$outcome <- outcome_label
  out <- out[, c("outcome", setdiff(names(out), "outcome"))]
  out
}

score_summary_all <- rbind(
  get_score_summary(score_mlb_boot_final, "Reach MLB"),
  get_score_summary(score_ret_boot_final, "Retire without MLB")
)

score_contrasts_all <- rbind(
  get_score_contrasts(score_mlb_boot_final, "Reach MLB"),
  get_score_contrasts(score_ret_boot_final, "Retire without MLB")
)

print(score_summary_all)
print(score_contrasts_all)

write_out(score_summary_all, "Table2_static_model_score_B100_summary.csv")
write_out(score_contrasts_all, "Table2_static_model_score_B100_contrasts.csv")

# Also save raw printed output for easy manuscript checking
capture.output(
  summary(score_mlb_boot_final),
  file = file.path(out_dir, "MLB_score_B100_summary_printed.txt")
)

capture.output(
  summary(score_ret_boot_final),
  file = file.path(out_dir, "Retire_score_B100_summary_printed.txt")
)

# -----------------------------
# 6) Time-varying model summary
# -----------------------------

# If tv_combined_summary already exists, use it.
# Otherwise reconstruct it from tv_mlb_combined and tv_ret_combined.

if (!exists("tv_combined_summary")) {
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
}

tv_combined_summary$LRT <- round(tv_combined_summary$LRT, 3)
tv_combined_summary$p_value_formatted <- formatC(
  tv_combined_summary$p_value,
  format = "e",
  digits = 3
)

print(tv_combined_summary)

write_out(tv_combined_summary, "Table3_time_varying_model_LRT_summary.csv")

# -----------------------------
# 7) Time-specific time-varying effects
# -----------------------------

format_time_effects <- function(x, outcome_label) {
  out <- x
  out$outcome <- outcome_label
  out$HR_at_time <- round(out$HR_at_time, 3)
  out$coef_at_time <- round(out$coef_at_time, 3)
  out$beta_main <- round(out$beta_main, 3)
  out$gamma_logtime <- round(out$gamma_logtime, 3)
  out <- out[, c(
    "outcome", "term", "time",
    "beta_main", "gamma_logtime",
    "coef_at_time", "HR_at_time"
  )]
  out
}

time_effects_all <- rbind(
  format_time_effects(mlb_time_effects, "Reach MLB"),
  format_time_effects(ret_time_effects, "Retire without MLB")
)

time_effects_short <- time_effects_all[
  time_effects_all$time %in% c(3, 5, 10),
]

print(time_effects_all)
print(time_effects_short)

write_out(time_effects_all, "Table4_time_specific_effects_full.csv")
write_out(time_effects_short, "Table4_time_specific_effects_short_3_5_10.csv")

# -----------------------------
# 8) Static vs time-varying prediction comparison
# -----------------------------

if (!exists("static_vs_tv_delta")) {
  stop("static_vs_tv_delta not found. Run the static-vs-time-varying comparison first.")
}

static_vs_tv_public <- static_vs_tv_delta

# Convert to percentage points for easier public-facing interpretation
delta_cols <- grep("_delta$", names(static_vs_tv_public), value = TRUE)
prob_cols <- grep("_static$|_tv$", names(static_vs_tv_public), value = TRUE)

static_vs_tv_percent <- static_vs_tv_public
static_vs_tv_percent[, c(prob_cols, delta_cols)] <- lapply(
  static_vs_tv_percent[, c(prob_cols, delta_cols)],
  function(x) round(100 * x, 1)
)

print(static_vs_tv_percent)

write_out(static_vs_tv_public, "Table5_static_vs_time_varying_prediction_comparison_raw.csv")
write_out(static_vs_tv_percent, "Table5_static_vs_time_varying_prediction_comparison_percent.csv")

# Max absolute delta summary
if (!exists("max_delta_summary")) {
  max_delta_summary <- data.frame(
    outcome = unique(static_vs_tv_delta$outcome),
    max_abs_delta = sapply(unique(static_vs_tv_delta$outcome), function(out) {
      dat <- static_vs_tv_delta[static_vs_tv_delta$outcome == out, delta_cols]
      max(abs(as.matrix(dat)), na.rm = TRUE)
    })
  )
}

max_delta_summary$max_abs_delta_percent_points <- round(100 * max_delta_summary$max_abs_delta, 1)

print(max_delta_summary)

write_out(max_delta_summary, "Table5_static_vs_time_varying_max_delta_summary.csv")

# -----------------------------
# 9) CIF sanity checks for representative profiles
# -----------------------------

mlb_tv_wide <- static_vs_tv_delta[
  static_vs_tv_delta$outcome == "MLB",
  c("profile", "t3_tv", "t5_tv", "t8_tv", "t10_tv")
]

ret_tv_wide <- static_vs_tv_delta[
  static_vs_tv_delta$outcome == "Retire",
  c("profile", "t3_tv", "t5_tv", "t8_tv", "t10_tv")
]

tv_combined_probs <- merge(
  mlb_tv_wide,
  ret_tv_wide,
  by = "profile",
  suffixes = c("_MLB", "_Retire")
)

tv_combined_probs$sum_t3  <- tv_combined_probs$t3_tv_MLB  + tv_combined_probs$t3_tv_Retire
tv_combined_probs$sum_t5  <- tv_combined_probs$t5_tv_MLB  + tv_combined_probs$t5_tv_Retire
tv_combined_probs$sum_t8  <- tv_combined_probs$t8_tv_MLB  + tv_combined_probs$t8_tv_Retire
tv_combined_probs$sum_t10 <- tv_combined_probs$t10_tv_MLB + tv_combined_probs$t10_tv_Retire

tv_sanity_summary <- tv_combined_probs[, c("profile", "sum_t3", "sum_t5", "sum_t8", "sum_t10")]

tv_sanity_summary_percent <- tv_sanity_summary
tv_sanity_summary_percent[, -1] <- lapply(
  tv_sanity_summary_percent[, -1],
  function(x) round(100 * x, 1)
)

tv_sanity_flags <- tv_sanity_summary[
  tv_sanity_summary$sum_t3 > 1 |
    tv_sanity_summary$sum_t5 > 1 |
    tv_sanity_summary$sum_t8 > 1 |
    tv_sanity_summary$sum_t10 > 1,
]

print(tv_sanity_summary_percent)
print(tv_sanity_flags)

write_out(tv_sanity_summary, "Check_TV_combined_CIF_sums_raw.csv")
write_out(tv_sanity_summary_percent, "Check_TV_combined_CIF_sums_percent.csv")
write_out(tv_sanity_flags, "Check_TV_combined_CIF_sums_flags.csv")

# -----------------------------
# 10) Monotonicity checks
# -----------------------------

check_monotone <- function(row, prefix) {
  vals <- as.numeric(row[paste0(c("t3", "t5", "t8", "t10"), "_", prefix)])
  all(diff(vals) >= -1e-8)
}

monotonicity_checks <- data.frame(
  profile = static_vs_tv_delta$profile,
  outcome = static_vs_tv_delta$outcome,
  static_monotone = apply(static_vs_tv_delta, 1, check_monotone, prefix = "static"),
  tv_monotone = apply(static_vs_tv_delta, 1, check_monotone, prefix = "tv")
)

monotonicity_flags <- monotonicity_checks[
  !monotonicity_checks$static_monotone |
    !monotonicity_checks$tv_monotone,
]

print(monotonicity_checks)
print(monotonicity_flags)

write_out(monotonicity_checks, "Check_prediction_monotonicity.csv")
write_out(monotonicity_flags, "Check_prediction_monotonicity_flags.csv")

# -----------------------------
# 11) Final model specification table
# -----------------------------

final_model_spec <- data.frame(
  outcome = c("Reach MLB", "Retire without MLB"),
  static_selected_model = c(
    "OvPck_sc*BSp_sc*Type + OvPck_sc*newPOS + Age + Bats + COVID_era",
    "OvPck_sc*BSp_sc*Type + OvPck_sc*BSp_sc + newPOS + Age"
  ),
  time_varying_terms = c(
    "Type + OvPck_sc, varying with log(time)",
    "Type + OvPck_sc + newPOS, varying with log(time)"
  ),
  final_prediction_model = c(
    "Time-varying Fine-Gray model",
    "Time-varying Fine-Gray model"
  ),
  notes = c(
    "BSp did not add time-varying signal after Type and OvPck_sc",
    "BSp did not add time-varying signal; newPOS retained as a block"
  )
)

print(final_model_spec)

write_out(final_model_spec, "Table6_final_model_specification.csv")

# -----------------------------
# 12) Save final reporting workspace
# -----------------------------

save(
  final_sample_summary,
  event_summary,
  type_summary,
  newpos_summary,
  continuous_summary,
  event_by_type,
  event_by_newpos,
  score_summary_all,
  score_contrasts_all,
  tv_combined_summary,
  time_effects_all,
  time_effects_short,
  static_vs_tv_public,
  static_vs_tv_percent,
  max_delta_summary,
  tv_sanity_summary,
  tv_sanity_flags,
  monotonicity_checks,
  monotonicity_flags,
  final_model_spec,
  file = file.path(out_dir, "FinalReportingTables.RData")
)

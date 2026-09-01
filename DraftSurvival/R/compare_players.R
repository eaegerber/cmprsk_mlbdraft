############################################################
# compare_players.R
#
# UI + server helpers for the DraftSurvival "Compare Players"
# tab. This file deliberately reuses predict_player_risks()
# from prediction_helpers.R so the comparison feature uses the
# exact same final time-varying Fine-Gray models as Predict.
############################################################

cp_series_choices <- function(label_a = "Player A", label_b = "Player B") {
  setNames(
    c("a_mlb", "a_retire", "b_mlb", "b_retire"),
    c(
      paste0(label_a, " — Reach MLB"),
      paste0(label_a, " — Retire before MLB"),
      paste0(label_b, " — Reach MLB"),
      paste0(label_b, " — Retire before MLB")
    )
  )
}

cp_manual_profile_ui <- function(prefix, title, default_pick = 100,
                                 default_bonus = 1, default_slot = 1,
                                 default_type = "4Yr", default_pos = "IF",
                                 default_age = 21, default_bats = "R",
                                 default_covid = "Post-COVID") {
  tagList(
    textInput(
      paste0(prefix, "_name"),
      "Profile label",
      value = if (prefix == "cp_a") "Manual Profile A" else "Manual Profile B"
    ),
    numericInput(
      paste0(prefix, "_o"),
      "Overall Pick #",
      value = default_pick,
      min = 1,
      max = NA,
      step = 1
    ),
    numericInput(
      paste0(prefix, "_b"),
      "Signing Bonus (millions $)",
      value = default_bonus,
      min = 0,
      max = NA,
      step = 0.1
    ),
    numericInput(
      paste0(prefix, "_s"),
      "Slot Value (millions $)",
      value = default_slot,
      min = 0.1,
      max = NA,
      step = 0.1
    ),
    radioButtons(
      paste0(prefix, "_t"),
      "Player Type",
      choices = c(
        "4-year college" = "4Yr",
        "High school" = "HS",
        "Junior college" = "JC"
      ),
      selected = default_type
    ),
    radioButtons(
      paste0(prefix, "_p"),
      "Position",
      choices = c(
        "C" = "C",
        "IF" = "IF",
        "OF" = "OF",
        "LHP" = "LHP",
        "RHP" = "RHP"
      ),
      selected = default_pos,
      inline = TRUE
    ),
    numericInput(
      paste0(prefix, "_age"),
      "Age at Draft",
      value = default_age,
      min = 16,
      max = 25,
      step = 1
    ),
    radioButtons(
      paste0(prefix, "_bats"),
      "Bats",
      choices = c(
        "Right" = "R",
        "Left" = "L",
        "Switch" = "B"
      ),
      selected = default_bats,
      inline = TRUE
    ),
    radioButtons(
      paste0(prefix, "_covid"),
      "Draft era",
      choices = c(
        "Pre-COVID" = "Pre-COVID",
        "Post-COVID" = "Post-COVID"
      ),
      selected = default_covid,
      inline = TRUE
    )
  )
}


cp_player_input_ui <- function(prefix, title,
                               default_pick = 100,
                               default_bonus = 1,
                               default_slot = 1,
                               default_type = "4Yr",
                               default_pos = "IF",
                               default_age = 21,
                               default_bats = "R",
                               default_covid = "Post-COVID") {
  div(
    class = "compare-input-card",
    h4(title),
    radioButtons(
      paste0(prefix, "_source"),
      "Profile source",
      choices = c(
        "Historical player" = "historical",
        "Manual profile" = "manual"
      ),
      selected = "historical",
      inline = TRUE
    ),
    conditionalPanel(
      condition = sprintf("input.%s_source == 'historical'", prefix),
      selectizeInput(
        paste0(prefix, "_hist"),
        "Search historical player",
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
        options = list(
          placeholder = "Start typing a player name...",
          maxOptions = 50
        )
      ),
      p(
        class = "small-note",
        "Each choice represents one signed, complete, model-eligible draft record. ",
        "If a player has more than one eligible draft event, each event is treated as a separate profile. ",
        "An earlier unsigned draft event can be recreated with Manual profile."
      )
    ),
    conditionalPanel(
      condition = sprintf("input.%s_source == 'manual'", prefix),
      cp_manual_profile_ui(
        prefix = prefix,
        title = title,
        default_pick = default_pick,
        default_bonus = default_bonus,
        default_slot = default_slot,
        default_type = default_type,
        default_pos = default_pos,
        default_age = default_age,
        default_bats = default_bats,
        default_covid = default_covid
      ),
      p(
        class = "small-note",
        "Choose Pre-COVID or Post-COVID to control the draft-era setting used by the MLB-reaching model."
      )
    )
  )
}


compare_players_ui <- function() {
  tabPanel(
    "Compare Players",

    tags$style(HTML("
      .compare-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 16px;
        margin-bottom: 16px;
        align-items: start;
      }

      .compare-input-card,
      .compare-profile-card,
      .compare-chart-card {
        background: #ffffff;
        border-radius: 12px;
        padding: 16px;
        box-shadow: 0 1px 5px rgba(0,0,0,0.09);
      }

      .compare-input-card h4,
      .compare-profile-card h4,
      .compare-chart-card h4 {
        margin-top: 0;
        font-weight: 600;
      }

      .compare-warning {
        background: #fff8e8;
        border-left: 4px solid #f0ad4e;
        border-radius: 6px;
        padding: 10px 14px;
        margin: 10px 0 15px 0;
        color: #555555;
      }

      .compare-takeaway {
        background: #eef6fb;
        border-left: 4px solid #2c7fb8;
        border-radius: 6px;
        padding: 12px 14px;
        margin-bottom: 16px;
      }

      .compare-action {
        margin: 4px 0 18px 0;
      }

      .compare-results-wrap {
        width: 100%;
        max-width: 1250px;
        margin: 0 auto 24px auto;
      }

      .compare-analysis-grid {
        display: grid;
        grid-template-columns: minmax(280px, 0.72fr) minmax(560px, 1.8fr);
        gap: 18px;
        align-items: start;
      }

      .compare-profile-stack {
        display: grid;
        grid-template-columns: 1fr;
        gap: 14px;
      }

      .compare-profile-header {
        display: flex;
        align-items: flex-start;
        justify-content: space-between;
        gap: 10px;
        margin-bottom: 12px;
      }

      .compare-profile-header h4 {
        margin-bottom: 3px;
      }

      .compare-profile-subtitle {
        color: #666666;
        font-size: 12px;
        line-height: 1.35;
      }

      .compare-badge-row {
        display: flex;
        flex-wrap: wrap;
        gap: 6px;
        margin-bottom: 12px;
      }

      .compare-badge {
        display: inline-block;
        border-radius: 999px;
        padding: 4px 9px;
        background: #f1f3f5;
        color: #4d4d4d;
        font-size: 11px;
        font-weight: 600;
      }

      .compare-feature-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 9px;
      }

      .compare-feature-item {
        background: #f7f8fa;
        border-radius: 8px;
        padding: 9px 10px;
        min-width: 0;
      }

      .compare-feature-label {
        color: #757575;
        font-size: 11px;
        line-height: 1.2;
        margin-bottom: 3px;
      }

      .compare-feature-value {
        color: #222222;
        font-size: 13px;
        font-weight: 600;
        overflow-wrap: anywhere;
      }

      .compare-chart-card {
        min-width: 0;
      }

      .compare-chart-controls {
        background: #f7f8fa;
        border-radius: 9px;
        padding: 10px 12px 4px 12px;
        margin-bottom: 8px;
      }

      .compare-chart-controls .control-label {
        font-size: 12px;
        font-weight: 600;
        margin-bottom: 5px;
      }

      .compare-chart-controls .checkbox {
        margin-top: 3px;
        margin-bottom: 5px;
      }

      .compare-plot-wrap {
        width: 100%;
        min-height: 520px;
      }

      .compare-plot-wrap .plotly,
      .compare-plot-wrap .html-widget,
      .compare-plot-wrap .js-plotly-plot {
        width: 100% !important;
      }

      .compare-table-toggle {
        margin: 16px 0 8px 0;
        padding-top: 12px;
        border-top: 1px solid #e5e5e5;
      }

      .compare-table-wrap {
        margin-top: 10px;
        overflow-x: auto;
      }

      .compare-table-wrap table {
        width: 100%;
      }

      @media (max-width: 1000px) {
        .compare-analysis-grid {
          grid-template-columns: 1fr;
        }

        .compare-plot-wrap {
          min-height: 470px;
        }
      }

      @media (max-width: 900px) {
        .compare-grid {
          grid-template-columns: 1fr;
        }
      }

      @media (max-width: 600px) {
        .compare-feature-grid {
          grid-template-columns: 1fr;
        }

        .compare-plot-wrap {
          min-height: 430px;
        }
      }
    ")),

    div(
      class = "intro-card",
      h3("Compare Two Player Profiles"),
      p(
        "Compare two historical draft profiles, two manual profiles, or one of each. ",
        "Each line is a model-based cumulative probability over the first 10 years after the draft."
      ),
      p(
        class = "small-note",
        "Historical profiles use the recorded draft-day characteristics and draft-era setting for that record. ",
        "Manual profiles let you choose Pre-COVID or Post-COVID. ",
        "Solid lines show reaching MLB; dashed lines show retiring before MLB. ",
        "The remaining probability at each horizon is interpreted as Still Playing in MiLB."
      )
    ),

    div(
      class = "compare-grid",
      cp_player_input_ui(
        prefix = "cp_a",
        title = "Player A",
        default_pick = 100,
        default_bonus = 1,
        default_slot = 1,
        default_type = "4Yr",
        default_pos = "IF",
        default_age = 21,
        default_bats = "R",
        default_covid = "Post-COVID"
      ),
      cp_player_input_ui(
        prefix = "cp_b",
        title = "Player B",
        default_pick = 300,
        default_bonus = 0.5,
        default_slot = 0.6,
        default_type = "HS",
        default_pos = "OF",
        default_age = 18,
        default_bats = "L",
        default_covid = "Post-COVID"
      )
    ),

    uiOutput("cp_manual_warning"),

    div(
      class = "compare-action",
      actionButton(
        "cp_compare",
        "Compare Players",
        class = "btn-primary"
      )
    ),

    uiOutput("cp_result_placeholder"),
    uiOutput("cp_prediction_warning"),
    uiOutput("cp_takeaway"),
    uiOutput("cp_results_section")
  )
}


compare_players_server <- function(input, output, session, model_obj, lookup_bundle) {

  # Support both the intended bundle format and a plain data.frame, so the
  # app fails gracefully if the lookup artifact is regenerated differently.
  if (is.data.frame(lookup_bundle)) {
    players <- lookup_bundle
    support <- NULL
  } else {
    players <- lookup_bundle$players
    support <- lookup_bundle$support
  }

  required_player_cols <- c(
    "player_key", "display_label", "person_id", "Name", "Year", "Tm", "OvPck",
    "Bonus", "Slot", "Type", "newPOS", "Age", "Bats", "COVID_era"
  )

  missing_cols <- setdiff(required_player_cols, names(players))
  if (length(missing_cols) > 0) {
    stop(
      "player_lookup.rds is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # Named vector: label -> internal key. server = TRUE keeps ~9k options
  # out of the initial browser payload while still allowing name search.
  player_choices <- setNames(players$player_key, players$display_label)

  find_default_key <- function(name, year = NULL, fallback_index = 1L) {
    idx <- which(tolower(players$Name) == tolower(name))
    if (!is.null(year)) {
      idx <- idx[players$Year[idx] == year]
    }

    if (length(idx) == 0) {
      fallback_index <- min(max(1L, fallback_index), nrow(players))
      return(players$player_key[fallback_index])
    }

    players$player_key[idx[1]]
  }

  default_a <- find_default_key("Alex Bregman", 2015, 1L)
  default_b <- find_default_key("Kris Bryant", 2013, min(2L, nrow(players)))

  updateSelectizeInput(
    session,
    "cp_a_hist",
    choices = player_choices,
    selected = default_a,
    server = TRUE
  )

  updateSelectizeInput(
    session,
    "cp_b_hist",
    choices = player_choices,
    selected = default_b,
    server = TRUE
  )

  historical_profile <- function(key, side_label) {
    if (is.null(key) || length(key) == 0 || is.na(key) || key == "") {
      stop(side_label, ": choose a historical player.")
    }

    idx <- match(key, players$player_key)
    if (is.na(idx)) {
      stop(side_label, ": selected historical player was not found.")
    }

    row <- players[idx, , drop = FALSE]

    era <- as.character(row$COVID_era)
    if (!era %in% c("Pre-COVID", "Post-COVID")) {
      stop(side_label, ": historical draft era is not recognized: ", era)
    }

    same_person <- which(players$person_id == row$person_id)
    same_person <- same_person[order(players$Year[same_person], players$OvPck[same_person])]
    draft_count <- length(same_person)
    draft_occurrence <- match(idx, same_person)

    list(
      side = side_label,
      label = paste0(as.character(row$Name), " (", as.integer(row$Year), ")"),
      display_label = as.character(row$display_label),
      source = "historical",
      person_id = as.character(row$person_id),
      ovpck = as.numeric(row$OvPck),
      bonus = as.numeric(row$Bonus),
      slot = as.numeric(row$Slot),
      type = as.character(row$Type),
      newpos = as.character(row$newPOS),
      age = as.numeric(row$Age),
      bats = as.character(row$Bats),
      covid_era = era,
      year = as.integer(row$Year),
      team = as.character(row$Tm),
      draft_count = draft_count,
      draft_occurrence = draft_occurrence
    )
  }

  manual_profile <- function(prefix, side_label) {
    profile_label <- input[[paste0(prefix, "_name")]]
    if (is.null(profile_label) || is.na(profile_label) || trimws(profile_label) == "") {
      profile_label <- side_label
    }

    values <- list(
      side = side_label,
      label = trimws(profile_label),
      display_label = trimws(profile_label),
      source = "manual",
      person_id = NA_character_,
      ovpck = input[[paste0(prefix, "_o")]],
      bonus = input[[paste0(prefix, "_b")]],
      slot = input[[paste0(prefix, "_s")]],
      type = input[[paste0(prefix, "_t")]],
      newpos = input[[paste0(prefix, "_p")]],
      age = input[[paste0(prefix, "_age")]],
      bats = input[[paste0(prefix, "_bats")]],
      covid_era = input[[paste0(prefix, "_covid")]],
      year = NA_integer_,
      team = NA_character_,
      draft_count = NA_integer_,
      draft_occurrence = NA_integer_
    )

    required_numeric <- c("ovpck", "bonus", "slot", "age")
    for (nm in required_numeric) {
      if (is.null(values[[nm]]) || length(values[[nm]]) == 0 || is.na(values[[nm]])) {
        stop(side_label, ": ", nm, " must be provided.")
      }
    }

    if (values$ovpck < 1) stop(side_label, ": overall pick must be at least 1.")
    if (values$bonus < 0) stop(side_label, ": signing bonus must be non-negative.")
    if (values$slot <= 0) stop(side_label, ": slot value must be greater than 0.")
    if (values$age < 16 || values$age > 25) {
      stop(side_label, ": age must be between 16 and 25.")
    }
    if (!values$covid_era %in% c("Pre-COVID", "Post-COVID")) {
      stop(side_label, ": choose Pre-COVID or Post-COVID.")
    }

    values
  }

  get_profile <- function(prefix, side_label) {
    source_value <- input[[paste0(prefix, "_source")]]

    if (identical(source_value, "historical")) {
      historical_profile(
        input[[paste0(prefix, "_hist")]],
        side_label
      )
    } else {
      manual_profile(prefix, side_label)
    }
  }

  run_prediction <- function(profile) {
    pred <- predict_player_risks(
      model_obj = model_obj,
      ovpck = profile$ovpck,
      bonus = profile$bonus,
      slot = profile$slot,
      type = profile$type,
      newpos = profile$newpos,
      age = profile$age,
      bats = profile$bats,
      covid_era = profile$covid_era,
      horizons = 1:10
    )

    numeric_cols <- c("MLB", "Retire", "Unresolved")
    if (any(!is.finite(as.matrix(pred[, numeric_cols, drop = FALSE])))) {
      stop(profile$side, ": prediction returned a non-finite probability.")
    }

    # Do not reject a comparison solely because independently estimated CIFs
    # sum slightly above 1 at a later horizon. prediction_helpers.R preserves
    # the valid_sum flag and floors Unresolved at zero; the UI surfaces a
    # warning instead of altering the fitted models.
    pred
  }

  # ---- Soft extrapolation warnings for manual profiles ----

  range_warning <- function(side_label, field_label, value, support_range,
                            digits = 2, prefix = "") {
    if (is.null(support_range) || length(support_range) < 2 || is.na(value)) {
      return(NULL)
    }

    lo <- as.numeric(support_range[1])
    hi <- as.numeric(support_range[2])

    if (value < lo || value > hi) {
      paste0(
        side_label, ": ", field_label, " ", prefix,
        format(round(value, digits), nsmall = digits, trim = TRUE),
        " is outside the observed training range [",
        prefix, format(round(lo, digits), nsmall = digits, trim = TRUE),
        ", ", prefix, format(round(hi, digits), nsmall = digits, trim = TRUE),
        "]."
      )
    } else {
      NULL
    }
  }

  manual_support_warnings <- reactive({
    if (is.null(support)) {
      return(character(0))
    }

    warnings <- character(0)

    for (spec in list(c("cp_a", "Player A"), c("cp_b", "Player B"))) {
      prefix <- spec[1]
      side_label <- spec[2]

      if (!identical(input[[paste0(prefix, "_source")]], "manual")) {
        next
      }

      ovpck <- input[[paste0(prefix, "_o")]]
      bonus <- input[[paste0(prefix, "_b")]]
      slot <- input[[paste0(prefix, "_s")]]
      age <- input[[paste0(prefix, "_age")]]

      bsp <- if (!is.null(slot) && !is.na(slot) && slot > 0 &&
                 !is.null(bonus) && !is.na(bonus)) bonus / slot else NA_real_

      checks <- c(
        range_warning(side_label, "overall pick", ovpck, support$OvPck, digits = 0),
        range_warning(side_label, "signing bonus", bonus, support$Bonus, digits = 2, prefix = "$"),
        range_warning(side_label, "slot value", slot, support$Slot, digits = 2, prefix = "$"),
        range_warning(side_label, "bonus/slot ratio", bsp, support$BSp, digits = 2),
        range_warning(side_label, "age", age, support$Age, digits = 0)
      )

      warnings <- c(warnings, checks[!is.na(checks) & nzchar(checks)])
    }

    warnings
  })

  output$cp_manual_warning <- renderUI({
    warnings <- manual_support_warnings()
    if (length(warnings) == 0) {
      return(NULL)
    }

    div(
      class = "compare-warning",
      strong("Extrapolation warning: "),
      "The model will still generate a prediction, but at least one manual value is outside the range observed in the final training data.",
      tags$ul(lapply(warnings, tags$li))
    )
  })

  # Store the last explicit comparison. Profile inputs clear the result when
  # changed, so the visualization never silently represents stale values.
  comparison <- reactiveVal(NULL)

  observeEvent(list(
    input$cp_a_source, input$cp_b_source,
    input$cp_a_hist, input$cp_b_hist,
    input$cp_a_name, input$cp_b_name,
    input$cp_a_o, input$cp_b_o,
    input$cp_a_b, input$cp_b_b,
    input$cp_a_s, input$cp_b_s,
    input$cp_a_t, input$cp_b_t,
    input$cp_a_p, input$cp_b_p,
    input$cp_a_age, input$cp_b_age,
    input$cp_a_bats, input$cp_b_bats,
    input$cp_a_covid, input$cp_b_covid
  ), {
    if (!is.null(comparison())) {
      comparison(NULL)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$cp_compare, {
    result <- tryCatch({
      profile_a <- get_profile("cp_a", "Player A")
      profile_b <- get_profile("cp_b", "Player B")

      if (
        identical(profile_a$source, "historical") &&
        identical(profile_b$source, "historical") &&
        identical(input$cp_a_hist, input$cp_b_hist)
      ) {
        stop("Choose two different historical draft profiles.")
      }

      pred_a <- run_prediction(profile_a)
      pred_b <- run_prediction(profile_b)

      list(
        profile_a = profile_a,
        profile_b = profile_b,
        pred_a = pred_a,
        pred_b = pred_b
      )
    }, error = function(e) e)

    if (inherits(result, "error")) {
      showNotification(
        conditionMessage(result),
        type = "error",
        duration = 8
      )
      comparison(NULL)
      return()
    }

    comparison(result)
  })

  output$cp_result_placeholder <- renderUI({
    if (!is.null(comparison())) {
      return(NULL)
    }

    div(
      class = "model-note",
      "Choose two profiles above, then click Compare Players to view their projected MLB and retirement trajectories."
    )
  })

  output$cp_prediction_warning <- renderUI({
    x <- comparison()
    if (is.null(x)) {
      return(NULL)
    }

    invalid_a <- which(!x$pred_a$valid_sum)
    invalid_b <- which(!x$pred_b$valid_sum)

    if (length(invalid_a) == 0 && length(invalid_b) == 0) {
      return(NULL)
    }

    warning_lines <- character(0)

    if (length(invalid_a) > 0) {
      total_a <- x$pred_a$MLB + x$pred_a$Retire
      warning_lines <- c(
        warning_lines,
        paste0(
          x$profile_a$side, " (", x$profile_a$label,
          "): combined MLB + retirement CIF reaches ",
          round(100 * max(total_a[invalid_a]), 1), "% at the affected horizon(s)."
        )
      )
    }

    if (length(invalid_b) > 0) {
      total_b <- x$pred_b$MLB + x$pred_b$Retire
      warning_lines <- c(
        warning_lines,
        paste0(
          x$profile_b$side, " (", x$profile_b$label,
          "): combined MLB + retirement CIF reaches ",
          round(100 * max(total_b[invalid_b]), 1), "% at the affected horizon(s)."
        )
      )
    }

    div(
      class = "compare-warning",
      strong("Competing-risk sum note: "),
      "The MLB and retirement CIFs are estimated separately, and for this profile they sum slightly above 100% at one or more horizons. ",
      "The fitted models are unchanged; Still Playing in MiLB is floored at 0% at those horizons.",
      tags$ul(lapply(warning_lines, tags$li))
    )
  })

  format_type <- function(x) {
    unname(c("4Yr" = "4-year college", "HS" = "High school", "JC" = "Junior college")[x])
  }

  format_bats <- function(x) {
    unname(c("R" = "Right", "L" = "Left", "B" = "Switch")[x])
  }

  feature_item <- function(label, value) {
    div(
      class = "compare-feature-item",
      div(class = "compare-feature-label", label),
      div(class = "compare-feature-value", value)
    )
  }

  profile_card <- function(profile) {
    ratio <- profile$bonus / profile$slot

    source_badge <- if (identical(profile$source, "historical")) {
      "Historical draft profile"
    } else {
      "Manual profile"
    }

    draft_note <- if (
      identical(profile$source, "historical") &&
      !is.na(profile$draft_count) &&
      profile$draft_count > 1
    ) {
      paste0(
        "Draft event ", profile$draft_occurrence,
        " of ", profile$draft_count,
        " model-eligible records for this player"
      )
    } else if (identical(profile$source, "historical")) {
      paste0(profile$year, " ", profile$team, " · recorded draft event")
    } else {
      "User-entered draft-day characteristics"
    }

    historical_items <- if (identical(profile$source, "historical")) {
      list(
        feature_item("Draft year", as.character(profile$year)),
        feature_item("Team", profile$team)
      )
    } else {
      list()
    }

    div(
      class = "compare-profile-card",
      div(
        class = "compare-profile-header",
        div(
          h4(paste0(profile$side, ": ", profile$label)),
          div(class = "compare-profile-subtitle", draft_note)
        )
      ),
      div(
        class = "compare-badge-row",
        span(class = "compare-badge", source_badge),
        span(class = "compare-badge", profile$covid_era)
      ),
      do.call(
        div,
        c(
          list(class = "compare-feature-grid"),
          historical_items,
          list(
            feature_item("Overall pick", paste0("#", as.integer(profile$ovpck))),
            feature_item("Signing bonus", paste0("$", format(round(profile$bonus, 3), trim = TRUE), "M")),
            feature_item("Slot value", paste0("$", format(round(profile$slot, 3), trim = TRUE), "M")),
            feature_item("Bonus / slot", format(round(ratio, 2), nsmall = 2, trim = TRUE)),
            feature_item("Player type", format_type(profile$type)),
            feature_item("Position", profile$newpos),
            feature_item("Age at draft", as.integer(profile$age)),
            feature_item("Bats", format_bats(profile$bats))
          )
        )
      )
    )
  }

  output$cp_takeaway <- renderUI({
    x <- comparison()
    req(!is.null(x))

    pa <- x$pred_a
    pb <- x$pred_b

    a6 <- pa$MLB[pa$time == 6]
    b6 <- pb$MLB[pb$time == 6]
    diff <- pa$MLB - pb$MLB
    tol <- 0.005

    horizon_message <- if (abs(a6 - b6) <= tol) {
      paste0(
        "At 6 years, the model gives the two profiles very similar chances of reaching MLB (",
        round(100 * a6, 1), "% vs ", round(100 * b6, 1), "%)."
      )
    } else if (a6 > b6) {
      paste0(
        "At 6 years, ", x$profile_a$label,
        " has the higher predicted cumulative probability of reaching MLB (",
        round(100 * a6, 1), "% vs ", round(100 * b6, 1), "%)."
      )
    } else {
      paste0(
        "At 6 years, ", x$profile_b$label,
        " has the higher predicted cumulative probability of reaching MLB (",
        round(100 * b6, 1), "% vs ", round(100 * a6, 1), "%)."
      )
    }

    trajectory_message <- if (all(diff >= -tol) && any(diff > tol)) {
      paste0(
        "The projected MLB probability for ",
        x$profile_a$label,
        " stays at or above that of ",
        x$profile_b$label,
        " across the displayed 1-10 year horizon."
      )
    } else if (all(diff <= tol) && any(diff < -tol)) {
      paste0(
        "The projected MLB probability for ",
        x$profile_b$label,
        " stays at or above that of ",
        x$profile_a$label,
        " across the displayed 1-10 year horizon."
      )
    } else if (any(diff > tol) && any(diff < -tol)) {
      "The projected MLB curves cross, so which profile looks stronger depends on the time horizon."
    } else {
      "The projected MLB curves are very similar across the displayed horizon."
    }

    div(
      class = "compare-takeaway",
      strong("Comparison takeaway: "),
      horizon_message,
      " ",
      trajectory_message
    )
  })

  output$cp_results_section <- renderUI({
    x <- comparison()
    req(!is.null(x))

    series_choices <- cp_series_choices(x$profile_a$label, x$profile_b$label)

    div(
      class = "compare-results-wrap",

      div(
        class = "compare-analysis-grid",

        div(
          class = "compare-profile-stack",
          profile_card(x$profile_a),
          profile_card(x$profile_b)
        ),

        div(
          class = "compare-chart-card",
          h4("Projected Draft Outcomes"),
          div(
            class = "compare-chart-controls",
            checkboxGroupInput(
              "cp_visible_series",
              "Lines shown",
              choices = series_choices,
              selected = unname(series_choices)
            ),
            p(
              class = "small-note",
              "Hover over annual markers for exact cumulative probabilities. You can also click legend entries to hide or show traces."
            )
          ),
          div(
            class = "compare-plot-wrap",
            plotly::plotlyOutput("cp_plot", width = "100%", height = "520px")
          )
        )
      ),

      div(
        class = "compare-table-toggle",
        checkboxInput(
          "cp_show_table",
          "Show comparison table",
          value = FALSE
        )
      ),

      conditionalPanel(
        condition = "input.cp_show_table === true",
        div(
          class = "compare-table-wrap",
          h4("Comparison at key horizons"),
          tableOutput("cp_table")
        )
      ),

      p(
        class = "small-note",
        "These are model-based cumulative probabilities, not guarantees for an individual player. ",
        "A historical comparison uses draft-day characteristics only; it does not use later MLB performance."
      )
    )
  })

  output$cp_plot <- plotly::renderPlotly({
    x <- comparison()
    req(!is.null(x))

    visible <- input$cp_visible_series
    validate(
      need(length(visible) > 0, "Select at least one line to display.")
    )

    pa <- x$pred_a
    pb <- x$pred_b

    col_a <- "#2c7fb8"
    col_b <- "#d95f02"

    make_hover <- function(profile_label, outcome_label, time, probability) {
      paste0(
        "<b>", profile_label, "</b>",
        "<br>", outcome_label,
        "<br>Years since draft: ", time,
        "<br>Cumulative probability: ", sprintf("%.2f%%", 100 * probability)
      )
    }

    p <- plotly::plot_ly()

    if ("a_mlb" %in% visible) {
      p <- plotly::add_trace(
        p,
        x = pa$time,
        y = pa$MLB,
        type = "scatter",
        mode = "lines+markers",
        name = paste0(x$profile_a$label, " — Reach MLB"),
        line = list(color = col_a, width = 3, dash = "solid", shape = "hv"),
        marker = list(color = col_a, size = 6),
        text = make_hover(x$profile_a$label, "Reach MLB", pa$time, pa$MLB),
        hovertemplate = "%{text}<extra></extra>"
      )
    }

    if ("a_retire" %in% visible) {
      p <- plotly::add_trace(
        p,
        x = pa$time,
        y = pa$Retire,
        type = "scatter",
        mode = "lines+markers",
        name = paste0(x$profile_a$label, " — Retire before MLB"),
        line = list(color = col_a, width = 3, dash = "dash", shape = "hv"),
        marker = list(color = col_a, size = 6),
        text = make_hover(x$profile_a$label, "Retire before MLB", pa$time, pa$Retire),
        hovertemplate = "%{text}<extra></extra>"
      )
    }

    if ("b_mlb" %in% visible) {
      p <- plotly::add_trace(
        p,
        x = pb$time,
        y = pb$MLB,
        type = "scatter",
        mode = "lines+markers",
        name = paste0(x$profile_b$label, " — Reach MLB"),
        line = list(color = col_b, width = 3, dash = "solid", shape = "hv"),
        marker = list(color = col_b, size = 6),
        text = make_hover(x$profile_b$label, "Reach MLB", pb$time, pb$MLB),
        hovertemplate = "%{text}<extra></extra>"
      )
    }

    if ("b_retire" %in% visible) {
      p <- plotly::add_trace(
        p,
        x = pb$time,
        y = pb$Retire,
        type = "scatter",
        mode = "lines+markers",
        name = paste0(x$profile_b$label, " — Retire before MLB"),
        line = list(color = col_b, width = 3, dash = "dash", shape = "hv"),
        marker = list(color = col_b, size = 6),
        text = make_hover(x$profile_b$label, "Retire before MLB", pb$time, pb$Retire),
        hovertemplate = "%{text}<extra></extra>"
      )
    }

    p <- plotly::layout(
      p,
      hovermode = "closest",
      margin = list(l = 72, r = 20, b = 62, t = 30),
      legend = list(
        orientation = "h",
        x = 0,
        y = 1.13,
        xanchor = "left",
        yanchor = "bottom",
        font = list(size = 11)
      ),
      xaxis = list(
        title = "Years Since Draft",
        tickmode = "linear",
        tick0 = 1,
        dtick = 1,
        range = c(1, 10),
        fixedrange = FALSE,
        gridcolor = "#eeeeee",
        zeroline = FALSE
      ),
      yaxis = list(
        title = "Cumulative Probability",
        range = c(0, 1),
        tickformat = ".0%",
        dtick = 0.1,
        fixedrange = FALSE,
        gridcolor = "#eeeeee",
        zeroline = FALSE
      ),
      shapes = list(
        list(
          type = "line",
          x0 = 6,
          x1 = 6,
          y0 = 0,
          y1 = 1,
          line = list(color = "#bdbdbd", width = 1, dash = "dot")
        )
      ),
      annotations = list(
        list(
          x = 6,
          y = 1,
          text = "6-year reference",
          showarrow = FALSE,
          yshift = 10,
          font = list(size = 10, color = "#777777")
        )
      )
    )

    plotly::config(
      p,
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("lasso2d", "select2d")
    )
  })

  output$cp_table <- renderTable({
    x <- comparison()
    req(!is.null(x))

    horizons <- c(3, 5, 6, 8, 10)

    pa <- x$pred_a[x$pred_a$time %in% horizons, , drop = FALSE]
    pb <- x$pred_b[x$pred_b$time %in% horizons, , drop = FALSE]

    pa <- pa[match(horizons, pa$time), , drop = FALSE]
    pb <- pb[match(horizons, pb$time), , drop = FALSE]

    data.frame(
      "Years Since Draft" = horizons,
      "A: Reach MLB" = paste0(round(100 * pa$MLB, 1), "%"),
      "B: Reach MLB" = paste0(round(100 * pb$MLB, 1), "%"),
      "A: Retire before MLB" = paste0(round(100 * pa$Retire, 1), "%"),
      "B: Retire before MLB" = paste0(round(100 * pb$Retire, 1), "%"),
      "A: Still Playing in MiLB" = paste0(round(100 * pa$Unresolved, 1), "%"),
      "B: Still Playing in MiLB" = paste0(round(100 * pb$Unresolved, 1), "%"),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
}

############################################################
# compare_players.R
#
# UI + server helpers for the DraftSurvival "Compare Players"
# tab. This file deliberately reuses predict_player_risks()
# from prediction_helpers.R so the comparison feature uses the
# exact same final time-varying Fine-Gray models as Predict.
############################################################

cp_manual_profile_ui <- function(prefix, title, default_pick = 100,
                                 default_bonus = 1, default_slot = 1,
                                 default_type = "4Yr", default_pos = "IF",
                                 default_age = 21, default_bats = "R") {
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
                               default_bats = "R") {
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
        "Historical choices are restricted to signed, complete draft records eligible for the final model."
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
        default_bats = default_bats
      ),
      p(
        class = "small-note",
        "Manual profiles use the Post-COVID model setting, matching the current Predict tab."
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
      .compare-summary-card {
        background: #ffffff;
        border-radius: 10px;
        padding: 15px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.08);
      }

      .compare-input-card h4,
      .compare-profile-card h4,
      .compare-summary-card h4 {
        margin-top: 0;
        font-weight: 600;
      }

      .compare-profile-line {
        margin-bottom: 4px;
      }

      .compare-summary-row {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 14px;
        margin-bottom: 18px;
      }

      .compare-metric-grid {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 8px;
        margin-top: 10px;
      }

      .compare-metric {
        background: #f7f8fa;
        border-radius: 8px;
        padding: 10px;
        text-align: center;
      }

      .compare-metric-label {
        font-size: 12px;
        color: #666666;
        min-height: 34px;
      }

      .compare-metric-value {
        font-size: 22px;
        font-weight: 700;
        margin-top: 3px;
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
        max-width: 1050px;
        margin: 0 auto 20px auto;
      }

      .compare-plot-wrap {
        width: 100%;
        aspect-ratio: 1.75 / 1;
        background: #ffffff;
        border-radius: 10px;
        padding: 12px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.08);
        margin: 0 auto 20px auto;
      }

      .compare-plot-wrap .shiny-plot-output {
        width: 100% !important;
        height: 100% !important;
      }

      .compare-results-wrap table {
        width: 100%;
      }

      @media (max-width: 900px) {
        .compare-grid,
        .compare-summary-row {
          grid-template-columns: 1fr;
        }

        .compare-metric-grid {
          grid-template-columns: 1fr;
        }
      }
    ")),

    div(
      class = "intro-card",
      h3("Compare Two Player Profiles"),
      p(
        "Compare two historical draft profiles, two manual profiles, or one of each. ",
        "The plot overlays each player's predicted cumulative probability of reaching MLB ",
        "(solid line) and retiring before reaching MLB (dashed line) over the first 10 years after the draft."
      ),
      p(
        class = "small-note",
        "Historical profiles use the player's recorded draft-day characteristics. ",
        "To make comparisons consistent, both historical and manual profiles are evaluated using the Post-COVID model setting. ",
        "The remaining probability at each horizon is shown as Still Playing in MiLB."
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
        default_bats = "R"
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
        default_bats = "L"
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
    uiOutput("cp_profiles"),
    uiOutput("cp_takeaway"),
    uiOutput("cp_six_year_cards"),
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
    "player_key", "display_label", "Name", "Year", "Tm", "OvPck",
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

    list(
      side = side_label,
      label = paste0(as.character(row$Name), " (", as.integer(row$Year), ")"),
      display_label = as.character(row$display_label),
      source = "historical",
      ovpck = as.numeric(row$OvPck),
      bonus = as.numeric(row$Bonus),
      slot = as.numeric(row$Slot),
      type = as.character(row$Type),
      newpos = as.character(row$newPOS),
      age = as.numeric(row$Age),
      bats = as.character(row$Bats),
      covid_era = "Post-COVID",
      year = as.integer(row$Year),
      team = as.character(row$Tm)
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
      ovpck = input[[paste0(prefix, "_o")]],
      bonus = input[[paste0(prefix, "_b")]],
      slot = input[[paste0(prefix, "_s")]],
      type = input[[paste0(prefix, "_t")]],
      newpos = input[[paste0(prefix, "_p")]],
      age = input[[paste0(prefix, "_age")]],
      bats = input[[paste0(prefix, "_bats")]],
      covid_era = "Post-COVID",
      year = NA_integer_,
      team = NA_character_
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

    if (!all(pred$valid_sum)) {
      stop(
        profile$side,
        ": this profile produced an invalid combined competing-risk prediction. ",
        "Try values closer to observed draft profiles."
      )
    }

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

  # Store the last explicit comparison. Inputs clear the result when changed,
  # so the plot never silently represents stale values.
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
    input$cp_a_bats, input$cp_b_bats
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

  profile_card <- function(profile) {
    ratio <- profile$bonus / profile$slot

    source_line <- if (identical(profile$source, "historical")) {
      paste0(
        profile$year, " ", profile$team,
        " | historical draft profile"
      )
    } else {
      "Manual profile | Post-COVID setting"
    }

    div(
      class = "compare-profile-card",
      h4(paste0(profile$side, ": ", profile$label)),
      div(class = "compare-profile-line", source_line),
      div(
        class = "compare-profile-line",
        paste0(
          profile$type, " | ", profile$newpos,
          " | bats ", profile$bats,
          " | age ", as.integer(profile$age)
        )
      ),
      div(
        class = "compare-profile-line",
        paste0(
          "Pick #", as.integer(profile$ovpck),
          " | bonus $", format(round(profile$bonus, 3), trim = TRUE), "M",
          " | slot $", format(round(profile$slot, 3), trim = TRUE), "M",
          " | bonus/slot ", round(ratio, 2)
        )
      )
    )
  }

  output$cp_profiles <- renderUI({
    x <- comparison()
    req(!is.null(x))

    div(
      class = "compare-grid",
      profile_card(x$profile_a),
      profile_card(x$profile_b)
    )
  })

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

  six_year_player_card <- function(profile, pred) {
    row6 <- pred[pred$time == 6, , drop = FALSE]

    div(
      class = "compare-summary-card",
      h4(profile$label),
      div(
        class = "compare-metric-grid",
        div(
          class = "compare-metric",
          div(class = "compare-metric-label", "Reach MLB by 6 years"),
          div(
            class = "compare-metric-value",
            style = "color:#2c7fb8;",
            paste0(round(100 * row6$MLB, 1), "%")
          )
        ),
        div(
          class = "compare-metric",
          div(class = "compare-metric-label", "Retire before MLB by 6 years"),
          div(
            class = "compare-metric-value",
            style = "color:#d95f02;",
            paste0(round(100 * row6$Retire, 1), "%")
          )
        ),
        div(
          class = "compare-metric",
          div(class = "compare-metric-label", "Still Playing in MiLB at 6 years"),
          div(
            class = "compare-metric-value",
            style = "color:#666666;",
            paste0(round(100 * row6$Unresolved, 1), "%")
          )
        )
      )
    )
  }

  output$cp_six_year_cards <- renderUI({
    x <- comparison()
    req(!is.null(x))

    div(
      class = "compare-summary-row",
      six_year_player_card(x$profile_a, x$pred_a),
      six_year_player_card(x$profile_b, x$pred_b)
    )
  })

  output$cp_results_section <- renderUI({
    req(!is.null(comparison()))

    div(
      class = "compare-results-wrap",

      div(
        class = "compare-plot-wrap",
        plotOutput("cp_plot", width = "100%", height = "100%")
      ),

      h4("Comparison at key horizons"),

      tableOutput("cp_table"),

      p(
        class = "small-note",
        "These are model-based cumulative probabilities, not guarantees for an individual player. ",
        "A historical comparison describes the model's prediction from draft-day characteristics; it does not use later MLB performance."
      )
    )
  })

  output$cp_plot <- renderPlot({
    x <- comparison()
    req(!is.null(x))

    pa <- x$pred_a
    pb <- x$pred_b

    col_a <- "#2c7fb8"
    col_b <- "#d95f02"

    par(mar = c(4.5, 4.5, 3.5, 1))

    plot(
      pa$time,
      pa$MLB,
      type = "s",
      lty = 1,
      lwd = 2.4,
      col = col_a,
      ylim = c(0, 1),
      xaxt = "n",
      xlab = "Years Since Draft",
      ylab = "Cumulative Probability",
      main = "Projected Draft Outcomes: Player Comparison"
    )

    axis(1, at = 1:10)

    lines(
      pa$time,
      pa$Retire,
      type = "s",
      lty = 2,
      lwd = 2.4,
      col = col_a
    )

    lines(
      pb$time,
      pb$MLB,
      type = "s",
      lty = 1,
      lwd = 2.4,
      col = col_b
    )

    lines(
      pb$time,
      pb$Retire,
      type = "s",
      lty = 2,
      lwd = 2.4,
      col = col_b
    )

    abline(v = 6, lty = 3, col = "gray60")

    legend(
      "topleft",
      legend = c(x$profile_a$label, x$profile_b$label),
      col = c(col_a, col_b),
      lty = 1,
      lwd = 2.4,
      bty = "n",
      cex = 0.9
    )

    legend(
      "bottomright",
      legend = c("Reach MLB", "Retire before MLB"),
      col = c("black", "black"),
      lty = c(1, 2),
      lwd = 2,
      bty = "n",
      cex = 0.9
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

############################################################
# test_predict_interactive_plot.R
#
# Lightweight UI smoke test for the Predict-tab Plotly graph.
#
# Run from repository root:
#   Rscript DraftSurvival/scripts/test_predict_interactive_plot.R
############################################################

app_path <- file.path("DraftSurvival", "app.R")

if (!file.exists(app_path)) {
  stop(
    "Could not find ", app_path,
    ". Run this script from the repository root."
  )
}

# Parsing catches R syntax errors without launching Shiny.
parse(file = app_path)

app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

required_patterns <- c(
  'plotly::plotlyOutput("distPlot"',
  'output$distPlot <- plotly::renderPlotly({',
  'name = "Reach MLB"',
  'name = "Retire before MLB"',
  'hovertemplate = "%{text}<extra></extra>"',
  'shape = "hv"',
  'if (isTRUE(input$a5))',
  'checkboxInput(',
  '"showPredictTable"',
  '"Show probability table"',
  'h4("Projected Draft Outcomes")',
  'condition = "input.showPredictTable === true"',
  'tableOutput("riskTable")',
  '"Estimated model-based cumulative probabilities at selected years after the draft."',
  '"<br>Cumulative percent: "',
  'title = "Cumulative Percent"'
)

missing_patterns <- required_patterns[
  !vapply(required_patterns, grepl, logical(1), x = app_text, fixed = TRUE)
]

if (length(missing_patterns) > 0) {
  stop(
    "Predict interactive-plot smoke test failed. Missing expected code:\n- ",
    paste(missing_patterns, collapse = "\n- ")
  )
}

if (grepl('plotOutput("distPlot"', app_text, fixed = TRUE)) {
  stop("Static plotOutput for distPlot is still present.")
}

if (grepl('output$distPlot <- renderPlot({', app_text, fixed = TRUE)) {
  stop("Static renderPlot for distPlot is still present.")
}

cat("\nPredict interactive plot smoke test PASSED.\n")
cat("Plotly output/rendering: PASS\n")
cat("Interactive hover tooltips: PASS\n")
cat("Legend-toggleable MLB/retirement traces: PASS\n")
cat("50% reference-line option preserved: PASS\n")
cat("Optional predicted-probabilities table toggle: PASS\n")
cat("Predict results layout formatting: PASS\n")
cat("Gerber-requested wording updates: PASS\n")

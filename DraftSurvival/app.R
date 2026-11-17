#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(cmprsk)
library(shinythemes)

# Load final app model and prediction helpers once at startup
final_app_model <- readRDS(file.path("model_objects", "final_app_model.rds"))
source(file.path("R", "prediction_helpers.R"))

# Compare Players
source(file.path("R", "compare_players.R"))
player_lookup_bundle <- readRDS(
  file.path("model_objects", "player_lookup.rds")
)

info_label <- function(label, tooltip) {
  tagList(
    label,
    tags$span(
      class = "info-icon",
      `data-toggle` = "tooltip",
      `data-placement` = "right",
      `data-html` = "true",
      tabindex = "0",
      title = tooltip,
      "\u24D8"
    )
  )
}

# Define UI for application that draws survival curves
ui <- fluidPage(
  theme = shinytheme("cosmo"),

  tags$head(includeHTML(("google-analytics.html")),
  
  tags$script(HTML("
      $(document).ready(function(){
        function adjustButtonText() {
          var width = $(window).width();
          
          if (width <= 900) {
            $('#autofill').text('Tom Brady?');
          } else {
            $('#autofill').text('Should Tom Brady have played baseball? (Assume average Round 18 Bonus of $36k)');
          }
        }

        adjustButtonText();  // adjust text when the page loads
        
        $(window).resize(adjustButtonText);  // adjust text when the window is resized
      });
    ")),
  
  tags$style(type = 'text/css', "
  #autofill {
    white-space: normal;
    width: 100%;  /* optional: makes sure the button stretches across the sidebar */
  }
"),
  
  tags$style(HTML("
    body {
      background-color: #f7f8fa;
    }

    .intro-card {
      background: #ffffff;
      border-radius: 10px;
      padding: 18px 22px;
      margin-bottom: 18px;
      box-shadow: 0 1px 4px rgba(0,0,0,0.08);
      border-left: 5px solid #2c7fb8;
    }

    .intro-card h3 {
      margin-top: 0;
      margin-bottom: 8px;
      font-weight: 600;
    }

    .small-note {
      color: #666666;
      font-size: 13px;
      margin-bottom: 0;
    }

    .input-section {
      background: #ffffff;
      border-radius: 10px;
      padding: 15px;
      margin-bottom: 15px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.06);
    }

    .input-section h4 {
      margin-top: 0;
      font-weight: 600;
      color: #333333;
      border-bottom: 1px solid #eeeeee;
      padding-bottom: 6px;
    }

    .risk-card-row {
      display: flex;
      gap: 12px;
      flex-wrap: wrap;
      margin-bottom: 18px;
    }

    .risk-card {
      background: #ffffff;
      border-radius: 10px;
      padding: 14px 16px;
      min-width: 180px;
      flex: 1;
      box-shadow: 0 1px 4px rgba(0,0,0,0.08);
    }

    .risk-label {
      font-size: 13px;
      color: #666666;
      margin-bottom: 4px;
    }

    .risk-value {
      font-size: 26px;
      font-weight: 700;
    }

    .risk-value.mlb {
      color: #2c7fb8;
    }

    .risk-value.retire {
      color: #d95f02;
    }

    .risk-value.unresolved {
      color: #666666;
    }

    .model-note {
      background: #fffdf5;
      border-left: 4px solid #f0ad4e;
      padding: 10px 14px;
      border-radius: 6px;
      font-size: 13px;
      color: #555555;
      margin-top: 15px;
    }

    table {
      background-color: #ffffff;
    }
    
    .golden-plot-wrap {
      width: 100%;
      max-width: 900px;
      aspect-ratio: 1.7 / 1;
      background: #ffffff;
      border-radius: 10px;
      padding: 12px 14px 8px 14px;
      box-shadow: 0 1px 4px rgba(0,0,0,0.08);
      margin: 0 auto 12px auto;
    }
    
    .golden-plot-wrap .shiny-plot-output,
    .golden-plot-wrap .plotly,
    .golden-plot-wrap .html-widget,
    .golden-plot-wrap .js-plotly-plot {
      width: 100% !important;
      height: 100% !important;
    }
    
    .profile-summary {
      background: #ffffff;
      border-radius: 8px;
      padding: 10px 14px;
      margin-bottom: 14px;
      color: #444444;
      box-shadow: 0 1px 3px rgba(0,0,0,0.06);
    }

    .predict-results-header {
      display: flex;
      align-items: baseline;
      justify-content: space-between;
      gap: 12px;
      margin: 2px 0 8px 0;
    }

    .predict-results-header h4 {
      margin: 0;
      font-weight: 600;
      color: #333333;
    }

    .predict-results-header .small-note {
      text-align: right;
    }

    .predict-table-toggle {
      background: #ffffff;
      border-radius: 8px;
      padding: 10px 14px;
      margin: 8px 0 10px 0;
      box-shadow: 0 1px 3px rgba(0,0,0,0.06);
    }

    .predict-table-toggle .form-group {
      margin-bottom: 0;
    }

    .predict-table-wrap {
      background: #ffffff;
      border-radius: 10px;
      padding: 14px 16px;
      margin-bottom: 16px;
      box-shadow: 0 1px 4px rgba(0,0,0,0.08);
      overflow-x: auto;
    }

    .predict-table-wrap h4 {
      margin-top: 0;
      font-weight: 600;
    }

    @media (max-width: 700px) {
      .predict-results-header {
        display: block;
      }

      .predict-results-header .small-note {
        text-align: left;
        margin-top: 4px;
      }
    }

    .info-icon {
      display: inline-block;
      margin-left: 6px;
      color: #2c7fb8;
      cursor: help;
      font-size: 14px;
      font-weight: 600;
    }
    
    .tooltip-inner {
      max-width: 320px;
      text-align: left;
      font-size: 13px;
      line-height: 1.35;
    }
  ")),
  
  tags$script(HTML("
  $(document).ready(function(){
    $('[data-toggle=\"tooltip\"]').tooltip({
      container: 'body',
      html: true
    });
  });
"))
  ),          
                
    navbarPage(
      "Competing Risks of MLB Draft Data",
      
      tabPanel("Predict",
               # # Application title
               # titlePanel(h3("Projected Player Draft Outcomes")),
               # 
               # # add white space
               # headerPanel(""),
               
               # Link
               #uiOutput("description1"),
               div(
                 class = "intro-card",
                 h3("MLB Draft Outcome Projection"),
                 p("The interactive plot below displays the predicted cumulative probability across each year after a player is drafted of both reaching MLB (blue solid) and retiring before MLB (orange dashed), based on the draft-day factors defined on the left."),
                 p(class = "small-note", "Predictions are based on signed MLB draft picks from 2012–2024 using competing risks models. For more details on the project, please see the Project Information tab above."),
                 p(class = "small-note", "Headline values show 6-year projections to align with minor league free agency; the table below includes additional 3-, 5-, 8-, and 10-year horizons.")
               ),
               
               # add white space
               headerPanel(""),
               
               # Sidebar 
               sidebarLayout(
                 sidebarPanel(
                   
                   div(
                     class = "input-section",
                     h4("Draft information"),
                     numericInput("o1",
                                  info_label(
                                    "Overall Pick #",
                                    "The player's overall draft pick number. Lower numbers indicate earlier selections. For example, pick 1 is the first overall pick, while pick 300 is a later-round selection."),
                                  value = 1,
                                  min = 1,
                                  max = 1200),  
                     numericInput("b1",
                                  info_label(
                                    "Signing Bonus (millions $)",
                                    "The player's signing bonus, entered in millions of dollars. For example, enter 2.5 for a $2.5 million bonus, or 0.15 for $150,000."),
                                  value = 10,
                                  min = 0,
                                  max = NA,
                                  step = .1),
                     numericInput("s1",
                                  info_label(
                                    "Slot Value (millions $)",
                                    "The assigned MLB draft slot value for the pick, entered in millions of dollars. These are pre-determined before each draft and usually published on MLB.com. The minimum slot value in the MLB draft is $100,000. The model uses the bonus-to-slot ratio to estimate whether a player signed above or below slot."),
                                  value = 10,
                                  min = 0.1,
                                  max = NA,
                                  step = .1),
                     textOutput("bonusSlotText")
                   ),
                   
                   div(
                     class = "input-section",
                     h4("Player Profile"),
                     # selectInput("t1",
                     #             "Type of Draftee",
                     #             choices = c("4Yr", "HS", "JC")),
                     radioButtons(
                       "t1",
                       info_label(
                         "Player Type",
                         "The player's draft source. 4-year college players, high school players, and junior college players often follow different development timelines."),
                       choices = c(
                         "4-year college" = "4Yr",
                         "High school" = "HS",
                         "Junior college" = "JC"
                       ),
                       selected = "4Yr"
                     ),
                     # selectInput("p1",
                     #             "Position",
                     #             choices = c("C", "IF", "OF", "LHP", "RHP")),
                     radioButtons(
                       "p1",
                       info_label(
                         "Position",
                         "The player's defensive position group at the time of drafting. Pitchers are separated into left-handed pitchers and right-handed pitchers."),
                       choices = c(
                         "C" = "C",
                         "IF" = "IF",
                         "OF" = "OF",
                         "LHP" = "LHP",
                         "RHP" = "RHP"
                       ),
                       selected = "IF",
                       inline = TRUE
                     ),
                     numericInput("age1",
                                  info_label(
                                    "Age at Draft",
                                    "The player's age in years at the time of the draft. Younger players, especially high school players, may have longer development timelines."),
                                  value = 21,
                                  min = 16,
                                  max = 25,
                                  step = 1),
                     # selectInput("bats1",
                     #             "Bats",
                     #             choices = c("Right" = "R",
                     #                         "Left" = "L",
                     #                         "Switch" = "B"),
                     #             selected = "R"),
                     radioButtons(
                       "bats1",
                       info_label(
                         "Bats",
                         "The player's batting handedness: right-handed, left-handed, or switch hitter."),
                       choices = c(
                         "Right" = "R",
                         "Left" = "L",
                         "Switch" = "B"
                       ),
                       selected = "R",
                       inline = TRUE)
                     ),
                   div(
                     class = "input-section",
                     h4("Display options"),
                     checkboxInput("a5",
                                   info_label(
                                     "Add 50% line?",
                                     "Adds a horizontal reference line at 50% cumulative probability."),
                                   value = FALSE),
                     #checkboxInput("ay",
                     #              "Add Year Line?",
                     #              value = FALSE),
                     #numericInput("y1",
                     #             "Year",
                     #             value = 4,
                     #             min = 0,
                     #             max = 10),
                     actionButton("autofill", 'Should Tom Brady have played baseball? (Assume average Round 18 Bonus of $36k)'),
                     br(), br(),
                     actionButton("reset", "Reset to Default", style = "color: white; background-color: green;") 
                   ),
                   width = 3
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                   uiOutput("profileSummary"),
                   uiOutput("headlineRisk"),
                   div(
                     class = "predict-results-header",
                     h4("Projected Draft Outcomes"),
                     p(
                       class = "small-note",
                       "Hover for exact values · click legend entries to show or hide lines"
                     )
                   ),
                   div(
                     class = "golden-plot-wrap",
                     plotly::plotlyOutput("distPlot", width = "100%", height = "100%")
                   ),
                   div(
                     class = "predict-table-toggle",
                     checkboxInput(
                       "showPredictTable",
                       "Show probability table",
                       value = FALSE
                     )
                   ),
                   conditionalPanel(
                     condition = "input.showPredictTable === true",
                     div(
                       class = "predict-table-wrap",
                       h4("Predicted cumulative probabilities"),
                       tableOutput("riskTable"),
                       p(
                         class = "small-note",
                         "Estimated cumulative probabilities at selected years after the draft; these are model-based projections, not guarantees."
                       )
                     )
                   ),
                   width = 9
                 )
               )
               ),
      
      compare_players_ui(),
      
      tabPanel(
        "Project Information",

        div(
          class = "intro-card",
          h3("About the Project"),
          uiOutput("info")
        ),

        div(
          class = "intro-card",
          h3("Project Contributors"),
          uiOutput("eaeg")
        )
      )
    )

)

# Define server logic required to draw plot
server <- function(input, output, session) {

    compare_players_server(
    input = input,
    output = output,
    session = session,
    model_obj = final_app_model,
    lookup_bundle = player_lookup_bundle
  )
  
#  url <- a(" at this link.", href="http://dx.doi.org/10.13140/RG.2.2.28623.46249")
#  output$tab <- renderUI({
#    tagList("A poster with more details of the method and results can be found", url, " A manuscript is also being prepared for submission to the Journal of Sports Analytics.")
#  })
  
  linkedIn <- a(
    "LinkedIn",
    href = "https://www.linkedin.com/in/eric-gerber-42544255/",
    target = "_blank"
  )
  ResearchGate <- a(
    "ResearchGate",
    href = "https://www.researchgate.net/profile/Eric-Gerber-4",
    target = "_blank"
  )
  bbref <- a(
    "baseball-reference.com",
    href = "https://www.baseball-reference.com/",
    target = "_blank"
  )
  bbam <- a(
    "Baseball America",
    href = "https://www.baseballamerica.com/",
    target = "_blank"
  )

  output$info <- renderUI({
    tagList(
      p(
        "This project uses competing-risks survival modeling to study how MLB ",
        "draft-day characteristics relate to the time until a player reaches MLB ",
        "or retires before reaching MLB."
      ),
      p(
        "MLB draft data were manually collected from ", bbref,
        ". Slot values were manually collected from ", bbam, "."
      )
    )
  })

  output$eaeg <- renderUI({
    tagList(
      h4("Eric A. E. Gerber, PhD"),
      p(
        "Eric A. E. Gerber is an Assistant Teaching Professor at ",
        "Northeastern University in Boston, MA."
      ),
      p(
        "Dr. Gerber can be reached via ", linkedIn, " or ", ResearchGate, "."
      ),
      tags$hr(),
      h4("Manika Sakulsureeyadej"),
      p(
        "Manika Sakulsureeyadej is a Northeastern University graduate in ",
        "Data Science and Business Administration who contributed to this ",
        "project as a research assistant. She developed the Compare Players ",
        "functionality for the Shiny application, including historical and ",
        "manually entered player comparisons, interactive competing-risk ",
        "visualizations, comparison controls, input validation, and supporting tests."
      )
    )
  })
  
  
  output$bonusSlotText <- renderText({
    validate(
      need(input$s1 > 0, "")
    )
    
    ratio <- input$b1 / input$s1
    
    paste0("Bonus / slot ratio: ", round(ratio, 2))
  })
  
  observeEvent(input$autofill, {
    # These are sample values. Replace with your desired specific values.
    updateNumericInput(session, "o1", value = 507)
    updateNumericInput(session, "b1", value = .036)
    updateNumericInput(session, "s1", value = .1)
    updateRadioButtons(session, "t1", selected = "HS")
    updateRadioButtons(session, "p1", selected = "C")
    updateNumericInput(session, "age1", value = 17)
    updateRadioButtons(session, "bats1", selected = "R")
    updateCheckboxInput(session, "a5", value = FALSE)
    # Add similar lines for any other inputs you want to update.
  })
  
  observeEvent(input$reset, {
    # These are sample values. Replace with your desired specific values.
    updateNumericInput(session, "o1", value = 1)
    updateNumericInput(session, "b1", value = 10)
    updateNumericInput(session, "s1", value = 10)
    updateRadioButtons(session, "t1", selected = "4Yr")
    updateRadioButtons(session, "p1", selected = "C")
    updateNumericInput(session, "age1", value = 21)
    #updateSelectInput(session, "bats1", selected = "R")
    updateRadioButtons(session, "bats1", selected = "R" )
    updateCheckboxInput(session, "a5", value = FALSE)
    updateCheckboxInput(session, "showPredictTable", value = FALSE)
    # Add similar lines for any other inputs you want to update.
  })

  output$profileSummary <- renderUI({
    
    bonus_slot <- input$b1 / input$s1
    
    div(
      class = "profile-summary",
      strong("Current player: "),
      paste0(
        input$t1, ", ",
        input$p1, ", bats ", input$bats1,
        ", age ", input$age1,
        ", pick ", input$o1,
        ", bonus/slot = ", round(bonus_slot, 2)
      )
    )
  })
  
  output$headlineRisk <- renderUI({
    
    validate(
      need(input$s1 > 0, "Slot value must be greater than 0."),
      need(input$b1 >= 0, "Bonus must be non-negative."),
      need(input$o1 >= 1, "Overall pick must be at least 1."),
      need(input$age1 >= 16 && input$age1 <= 25, "Age must be between 16 and 25.")
    )
    
    headline_horizon <- 6
    
    pred <- predict_player_risks(
      model_obj = final_app_model,
      ovpck = input$o1,
      bonus = input$b1,
      slot = input$s1,
      type = input$t1,
      newpos = input$p1,
      age = input$age1,
      bats = input$bats1,
      covid_era = "Post-COVID",
      horizons = headline_horizon
    )
    
    div(
      class = "risk-card-row",
      div(
        class = "risk-card",
        div(class = "risk-label", paste0("Reach MLB by ", headline_horizon, " years")),
        div(class = "risk-value mlb", paste0(round(100 * pred$MLB, 1), "%"))
      ),
      div(
        class = "risk-card",
        div(class = "risk-label", paste0("Retire before MLB by ", headline_horizon, " years")),
        div(class = "risk-value retire", paste0(round(100 * pred$Retire, 1), "%"))
      ),
      div(
        class = "risk-card",
        div(class = "risk-label", paste0("Still playing in MiLB at ", headline_horizon, " years")),
        div(class = "risk-value unresolved", paste0(round(100 * pred$Unresolved, 1), "%"))
      )
    )
  })
  
  output$distPlot <- plotly::renderPlotly({

    validate(
      need(input$s1 > 0, "Slot value must be greater than 0."),
      need(input$b1 >= 0, "Bonus must be non-negative."),
      need(input$o1 >= 1, "Overall pick must be at least 1."),
      need(input$age1 >= 16 && input$age1 <= 25, "Age must be between 16 and 25.")
    )

    # Use annual predictions to match the existing Predict-tab trajectory.
    plot_horizons <- 1:10

    pred <- predict_player_risks(
      model_obj = final_app_model,
      ovpck = input$o1,
      bonus = input$b1,
      slot = input$s1,
      type = input$t1,
      newpos = input$p1,
      age = input$age1,
      bats = input$bats1,
      covid_era = "Post-COVID",
      horizons = plot_horizons
    )

    col_mlb <- "#2c7fb8"
    col_retire <- "#d95f02"

    make_hover <- function(outcome_label, time, probability) {
      paste0(
        "<b>", outcome_label, "</b>",
        "<br>Years since draft: ", time,
        "<br>Cumulative probability: ", sprintf("%.2f%%", 100 * probability)
      )
    }

    p <- plotly::plot_ly()

    p <- plotly::add_trace(
      p,
      x = pred$time,
      y = pred$MLB,
      type = "scatter",
      mode = "lines+markers",
      name = "Reach MLB",
      line = list(
        color = col_mlb,
        width = 3,
        dash = "solid",
        shape = "hv"
      ),
      marker = list(
        color = col_mlb,
        size = 6
      ),
      text = make_hover("Reach MLB", pred$time, pred$MLB),
      hovertemplate = "%{text}<extra></extra>"
    )

    p <- plotly::add_trace(
      p,
      x = pred$time,
      y = pred$Retire,
      type = "scatter",
      mode = "lines+markers",
      name = "Retire before MLB",
      line = list(
        color = col_retire,
        width = 3,
        dash = "dash",
        shape = "hv"
      ),
      marker = list(
        color = col_retire,
        size = 6
      ),
      text = make_hover(
        "Retire before MLB",
        pred$time,
        pred$Retire
      ),
      hovertemplate = "%{text}<extra></extra>"
    )

    reference_shapes <- if (isTRUE(input$a5)) {
      list(
        list(
          type = "line",
          x0 = min(plot_horizons),
          x1 = max(plot_horizons),
          y0 = 0.5,
          y1 = 0.5,
          line = list(
            color = "#777777",
            width = 2,
            dash = "dot"
          )
        )
      )
    } else {
      list()
    }

    p <- plotly::layout(
      p,
      hovermode = "closest",
      margin = list(l = 72, r = 20, b = 62, t = 52),
      legend = list(
        orientation = "h",
        x = 0,
        y = 1.02,
        xanchor = "left",
        yanchor = "bottom",
        font = list(size = 11),
        bgcolor = "rgba(255,255,255,0.85)"
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
      shapes = reference_shapes
    )

    plotly::config(
      p,
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("lasso2d", "select2d")
    )
  })

  output$riskTable <- renderTable({
    
    validate(
      need(input$s1 > 0, "Slot value must be greater than 0."),
      need(input$b1 >= 0, "Bonus must be non-negative."),
      need(input$o1 >= 1, "Overall pick must be at least 1."),
      need(input$age1 >= 16 && input$age1 <= 25, "Age must be between 16 and 25.")
    )
    
    # Keep table at main reporting horizons
    table_horizons <- c(3, 5, 8, 10)
    
    pred <- predict_player_risks(
      model_obj = final_app_model,
      ovpck = input$o1,
      bonus = input$b1,
      slot = input$s1,
      type = input$t1,
      newpos = input$p1,
      age = input$age1,
      bats = input$bats1,
      covid_era = "Post-COVID",
      horizons = table_horizons
    )
    
    out <- data.frame(
      "Years Since Draft" = as.integer(pred$time),
      "Reach MLB" = paste0(round(100 * pred$MLB, 1), "%"),
      "Retire before MLB" = paste0(round(100 * pred$Retire, 1), "%"),
      "Censored (Playing in MiLB)" = paste0(round(100 * pred$Unresolved, 1), "%"),
      check.names = FALSE
    )
    
    out
  }, striped = TRUE, bordered = TRUE, spacing = "s")
    
}

# Run the application 
shinyApp(ui = ui, server = server)

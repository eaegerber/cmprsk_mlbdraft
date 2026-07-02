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
      max-width: 800px;
      aspect-ratio: 1.618 / 1;
      background: #ffffff;
      border-radius: 10px;
      padding: 10px;
      box-shadow: 0 1px 4px rgba(0,0,0,0.08);
      margin: 0 auto 18px auto;
    }
    
    .golden-plot-wrap .shiny-plot-output {
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
                 p("The plot below displays the predicted risk across each year after a player is drafted of both reaching MLB (blue solid) and Retiring (red dashed) based on the draft day factors as defined on the left."),
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
                     class = "golden-plot-wrap",
                     plotOutput("distPlot", width = "100%", height = "100%")
                   ),
                   br(),
                   h4("Predicted cumulative probabilities"),
                   tableOutput("riskTable"),
                   p(
                     class = "small-note",
                     "Table values show the estimated probability of each outcome by selected years after the draft, not guarantees for individual players."
                   ),
                   width = 9
                 )
               )
               ),
      
      # tabPanel(
      #   "Compare Players",
      #   div(
      #     class = "intro-card",
      #     h3("Compare Players"),
      #     p("This feature is being updated for the new time-varying model and will be added in a future version.")
      #   )
      # ),
      
      tabPanel("Project Information",
               
               # Link to poster
               uiOutput("tab"),
               
               # add white space
               headerPanel(""),
               
               # information
               uiOutput("info"),
               
               # add white space
               headerPanel(""),
               
               uiOutput("eaeg")
               
               )
    )

)

# Define server logic required to draw plot
server <- function(input, output, session) {
  
  
#  url <- a(" at this link.", href="http://dx.doi.org/10.13140/RG.2.2.28623.46249")
#  output$tab <- renderUI({
#    tagList("A poster with more details of the method and results can be found", url, " A manuscript is also being prepared for submission to the Journal of Sports Analytics.")
#  })
  
#  linkedIn <- a("LinkedIn", href = "https://www.linkedin.com/in/eric-gerber-42544255/")
#  ResearchGate <- a("ResearchGate.", href = "https://www.researchgate.net/profile/Eric-Gerber-4")
  bbref <- a("baseball-reference.com", href = "https://www.baseball-reference.com/")
  bbam <- a("Baseball America", href = "https://www.baseballamerica.com/")
#  output$info <- renderUI({
#    tagList("Eric A. E. Gerber is an Assistant Teaching Professor at Northeastern University in Boston, MA and Migzhao Hu is an Assistant Professor at the Mayo Clinic in Rochester, MN. This project was supported in part by the Office of Grants, Research, and Sponsored Programs (GRaSP) at CSU Bakersfield as part of a faculty research funding program of the Research Council of the University (RCU). MLB draft data were manually collected from ", bbref, ". Slot Values were manually collected from ", bbam, ". \n\n The authors would also like to thank several students who had both tangible and intangible impacts upon the development of this work, including students Nelson Guirado (CSU Bakersfield) who performed most of the data collection, and Yash Jayaprakash, Nikhil Bommareddy, Kaamil Thobani, Ryan Monahan, Vamshi Pagidi, and Xi Chen (Northeastern University) who provided both good ideas and some technical support." )
#  })
  
    output$info <- renderUI({
      tagList("MLB draft data were manually collected from ", bbref, ". Slot Values were manually collected from ", bbam, "." )
    })
  
  
#  output$eaeg <- renderUI({
#    tagList("Dr. Gerber can be reached via ", linkedIn, " or ", ResearchGate)
#  })
  
  
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
  
  output$distPlot <- renderPlot({
    
    validate(
      need(input$s1 > 0, "Slot value must be greater than 0."),
      need(input$b1 >= 0, "Bonus must be non-negative."),
      need(input$o1 >= 1, "Overall pick must be at least 1."),
      need(input$age1 >= 16 && input$age1 <= 25, "Age must be between 16 and 25.")
    )
    
    # Use annual predictions for smoother app-facing plot
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
    
    plot(
      pred$time,
      pred$MLB,
      col = 4,
      type = "s",
      lty = 1,
      lwd = 2,
      ylim = c(0, 1),
      xaxt = "n",
      main = "Projected Draft Outcomes",
      ylab = "Cumulative Probability",
      xlab = "Years Since Draft"
    )
    
    axis(1, at = plot_horizons)
    
    points(
      pred$time,
      pred$Retire,
      col = 2,
      type = "s",
      lty = 2,
      lwd = 2
    )
    
    legend(
      "topleft",
      c("Reach MLB", "Retire before MLB"),
      lty = c(1, 2),
      col = c(4, 2),
      lwd = 2,
      bty = "n"
    )
    
    if (input$a5 == TRUE) {
      abline(h = .5, col = 1, lwd = 2)
    }
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

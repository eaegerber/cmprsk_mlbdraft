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

PlayerList <- c("Alex Bregman (2012)", "Alex Bregman (2015)", "Garrett Whitley (2015)", "Ryan Yarbrough (2013)", "Ryan Yarbrough (2014)", "Tyler Kolek (2014)")

# Define UI for application that draws survival curves
ui <- fluidPage(theme = shinytheme("cosmo"),

    tags$head(includeHTML(("google-analytics.html")),
              tags$script(HTML("
      $(document).ready(function(){
        function adjustButtonText() {
          var width = $(window).width();
          
          if (width <= 900) {
            $('#autofill').text('Tom Brady?');
          } else {
            $('#autofill').text('Should Tom Brady have played baseball? (Assume average Round 18 Bonus of $31k)');
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
")),            
                
    navbarPage(
      "Competing Risks of MLB Draft Data",
      tabPanel("Predict",
               # Application title
               titlePanel(h3("Predicting Player Risk")),
               
               # add white space
               headerPanel(""),
               
               # Link
               uiOutput("description1"),
               
               # add white space
               headerPanel(""),
               
               # Sidebar 
               sidebarLayout(
                 sidebarPanel(
                   numericInput("o1",
                                "Overall Pick #",
                                value = 1,
                                min = 1,
                                max = 1200),  
                   numericInput("b1",
                                "Bonus (in millions $)",
                                value = 10,
                                min = 0,
                                max = NA,
                                step = .1),  
                   numericInput("s1",
                                "Slot (in millions $)",
                                value = 10,
                                min = 0,
                                max = NA,
                                step = .1),
                   selectInput("t1",
                               "Type of Draftee",
                               choices = c("4Yr", "HS", "JC")),
                   selectInput("p1",
                               "Position",
                               choices = c("C", "IF", "OF", "LHP", "RHP")),
                   checkboxInput("a5",
                                 "Add 50% Line?",
                                 value = FALSE),
                   #checkboxInput("ay",
                   #              "Add Year Line?",
                   #              value = FALSE),
                   #numericInput("y1",
                   #             "Year",
                   #             value = 4,
                   #             min = 0,
                   #             max = 10),
                   actionButton("autofill", 'Should Tom Brady have played baseball? (Assume average Round 18 Bonus of $31k)'),
                   actionButton("reset", "Reset to Default", style = "color: white; background-color: green;"),
                   width = 3
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                   plotOutput("distPlot", width = "550px"),
                   width = 9
                 )
               )
               ),
      tabPanel("Compare",
               
               # Application title
               titlePanel(h2("Compare Two Players (Work in Progress)")),
               
               # add white space
               headerPanel(""),
               
               # Link
               uiOutput("description2"),
               
               # add white space
               headerPanel(""),
               
               sidebarLayout(
                 sidebarPanel(
                   selectInput("playerA",
                               "Pick Player A",
                               PlayerList),
                   selectInput("playerB",
                               "Pick Player B",
                               PlayerList),
                   h5("Alex Bregman reached MLB in his 2nd year."),
                   h5("Garret Whitley is still playing in MiLB."),
                   h5("Ryan Yarbrough reached MLB in his 5th year."),
                   h5("Tyler Kolek retired after his 5th year."),
                   checkboxInput("a6",
                                 "Add 50% Line?",
                                 value = FALSE),
                   width = 3
                 ),
                 
                 mainPanel(
                   plotOutput("distPlot2", width = "550px"),
                   width = 9
                 )
               )
               ),
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
  
  output$description1 <- renderUI({
    tagList("The plot below displays the predicted risk across each year after a player is drafted of both reaching MLB (blue solid) and retiring (red dashed) based on the draft day factors as defined on the left. A naive prediction of when an event may occur would be the year the risk surpasses 50%. For more details on the project, please see the Project Information tab above. 
            
            \n\n Please note these are the predicted risks under the Fine-Gray model; at the moment, generating individual predictions under the Accelerated-Failure Time model is too computationally intensive.")
  })
  
  output$description2 <- renderUI({
    tagList("The below plot compares the predicted risk for several example players. The chosen players illustrate a range of possible outcomes: Alex Bregman and Ryan Yarbrough both improved their chances of reaching MLB by re-entering the draft after being initially drafted, and subsequently did reach MLB. Tyler Kolek was a top draft pick, predicted to easily reach MLB, but retired without doing so. Garret Whitley, as of the running of this model, is still playing in the minors.")
  })
  
  observeEvent(input$autofill, {
    # These are sample values. Replace with your desired specific values.
    updateNumericInput(session, "o1", value = 507)
    updateNumericInput(session, "b1", value = .036)
    updateNumericInput(session, "s1", value = .1)
    updateSelectInput(session, "t1", selected = "HS")
    updateSelectInput(session, "p1", selected = "C")
    updateCheckboxInput(session, "a5", value = FALSE)
    # Add similar lines for any other inputs you want to update.
  })
  
  observeEvent(input$reset, {
    # These are sample values. Replace with your desired specific values.
    updateNumericInput(session, "o1", value = 1)
    updateNumericInput(session, "b1", value = 10)
    updateNumericInput(session, "s1", value = 10)
    updateSelectInput(session, "t1", selected = "4Yr")
    updateSelectInput(session, "p1", selected = "C")
    updateCheckboxInput(session, "a5", value = FALSE)
    # Add similar lines for any other inputs you want to update.
  })

    output$distPlot <- renderPlot({
        #load model
        load("modelfit_050224.RData")
        #Dataframe
        newdata <- data.frame(
        OvPck = rep(input$o1, 2),
        BSp = rep(input$b1/input$s1, 2),
        Type = factor(rep(input$t1, 2), levels = c("4Yr", "HS", "JC")),
        newPOS = factor(rep(input$p1, 2), levels = c("C", "IF", "LHP", "OF", "RHP")))
        
        #Old Data (in preparation for eventually doing AFT predictions?)
        olddf <- scale_df[,c("OvPck", "BSp", "Type", "newPOS")]
      
        #get predictions
        newdf1 <- model.matrix(~scale(OvPck, center = center.OvPck, scale = sd.OvPck)*scale(BSp, center = center.BSp, scale = sd.BSp)*Type + scale(OvPck, center = center.OvPck, scale = sd.OvPck)*newPOS, newdata)[,-1]
        colnames(newdf1) <- colnames(cov1)
        
        newdf2 <- model.matrix(~scale(OvPck, center = center.OvPck, scale = sd.OvPck)*scale(BSp, center = center.BSp, scale = sd.BSp)*Type + newPOS, newdata)[,-1]
        colnames(newdf2) <- colnames(cov2)
        
        playerpred1 <- predict(crr.model, newdf1)
        playerpred2 <- predict(crr.model2, newdf2)
        
        # draw the plot
        plot(playerpred1[,2], col = 4, type = "s", lty=1, lwd = 2, ylim = c(0,1), main = "Player Risk", ylab = "Predicted Risk", xlab = "Years")
        points(playerpred2[,2], col = 2, type = "s", lty=2, lwd = 2)
        legend("topleft", c("Reach MLB", "Retire"), lty=1:2, col=c(4,2), lwd = 2)
        if(input$a5 == TRUE){
          abline(h = .5, col = 1, lwd=2)
        }
        #if(input$ay == TRUE){
        #  abline(v = input$y1, col = 1, lwd =2)
        #}
    })
    
    
    output$distPlot2 <- renderPlot({
      #load model
      load("modelfit_050224.RData")
      
      playercomp1 <- model.matrix(~scale(OvPck, center = center.OvPck, scale = sd.OvPck)*scale(BSp, center = center.BSp, scale = sd.BSp)*Type + scale(OvPck, center = center.OvPck, scale = sd.OvPck)*newPOS, data.frame(
        OvPck = c(901,2,13,602,111,2),
        BSp = c(0/.1,5.9/7.4201,2.9596/2.9621,0/.1,.04/0.4714,6/6.8218
),
        Type = factor(c("HS","4Yr","HS","4Yr","4Yr","HS"), levels = c("4Yr", "HS", "JC")),
        newPOS = factor(c("IF","IF","OF","LHP","LHP","RHP"), levels = c("C", "IF", "LHP", "OF", "RHP"))))[,-1]
      colnames(playercomp1) <- colnames(cov1)
      
      playercomp2 <- model.matrix(~scale(OvPck, center = center.OvPck, scale = sd.OvPck)*scale(BSp, center = center.BSp, scale = sd.BSp)*Type + newPOS, data.frame(
        OvPck = c(901,2,13,602,111,2),
        BSp = c(0/.1,5.9/7.4201,2.9596/2.9621,0/.1,.04/0.4714,6/6.8218
        ),
        Type = factor(c("HS","4Yr","HS","4Yr","4Yr","HS"), levels = c("4Yr", "HS", "JC")),
        newPOS = factor(c("IF","IF","OF","LHP","LHP","RHP"), levels = c("C", "IF", "OF", "LHP", "RHP"))))[,-1]
      colnames(playercomp2) <- colnames(cov2)
      
      plA <- which(PlayerList == input$playerA)
      plB <- which(PlayerList == input$playerB)
      
      playercomp.pred1 <- predict(crr.model, playercomp1[c(plA,plB),])
      playercomp.pred2 <- predict(crr.model2, playercomp2[c(plA,plB),])
      
      plot(playercomp.pred1[,2], col = 4, type = "s", lty=1, lwd = 2, ylim = c(0,1), main = "Player Risk Comparison", ylab = "Predicted Risk", xlab = "Years")
      points(playercomp.pred2[,2], col = 2, type = "s", lty=2, lwd = 2)
      points(playercomp.pred1[,3], col = 5, type = "s", lty=3, lwd = 2)
      points(playercomp.pred2[,3], col = 7, type = "s", lty=4, lwd = 2)
      legend("topleft", c(paste(input$playerA,"Reach MLB"), paste(input$playerA, "Retire"), paste(input$playerB, "Reach MLB"), paste(input$playerB, "Retire")), lty=1:4, col=c(4,2,5,7), lwd = 2)
      if(input$a6 == TRUE){
        abline(h = .5, col = 1, lwd=2)
      }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

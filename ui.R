# UI

# Libraries

        library(shiny)
        library(shinydashboard)
        library(shinyjs)
        library(dplyr)
        library(ggplot2)
        library(plotly)
        library(rsm)
        library(DT)
        
        
        
myHeader <- dashboardHeader(disable=TRUE)

mySidebar <- dashboardSidebar(disable=TRUE)

myBody <- dashboardBody(
        
        useShinyjs(),
        
        fluidRow(
                
                column(width=8,
                       
                       shinyjs::hidden(
                               div(
                                       id="divPlotSurface",
                                       box(title = "Actual Surface", width = 12, 
                                           plotlyOutput("plotSurface")       
                                       )
                               )
                       ),
                       
                       div(
                               id="divOutput",
                               
                               box(title = "DOE Plot", width = 12,
                                   plotlyOutput("plotResults")
                               ),
                               
                               box(title = "DOE Data Table", width = 12, collapsible = TRUE, collapsed = FALSE,
                                   DT::dataTableOutput("dfDOE")    
                               )        
                       )
                ),
                
                
                
                column(width=4,
                       
                       fixedPanel(
                               
                               div(
                                       id="divMyUpdate", style="display:inline-block",
                                       actionButton(inputId = "myUpdate", label = "Experiment")        
                               ),
                               
                               div(
                                       id="divMyGuess", style="display:inline-block",
                                       actionButton(inputId = "myGuess", label = "Guess Optimal")        
                               ),
                               
                               shinyjs::hidden(
                                       div(
                                               id="divMyFinish", style="display:inline-block",
                                               actionButton(inputId = "myFinish", label = "Submit My Guess")        
                                       )
                               ),
                               
                               tags$br(), tags$br(),
                               
                               div(
                                       id="divType",
                                       
                                       selectInput(inputId = "myType", label = "Type", choices = c("Screening","Response Surface"), selected = "Screening"),
                                       
                                       conditionalPanel(
                                               condition = "input.myType == 'Response Surface'",
                                               selectInput(inputId = "myBlocks",  label = "Blocks", choices = c("Corners"=1, "Star"=2), selected = c(1,2), multiple = TRUE)
                                       )
                               ),
                               
                               
                               div(
                                       id="divX1", style="display:inline-block",
                                       sliderInput(inputId = "x1Min", label = "X1 Min", min = 0, max = 100, value = 20, step = 5),
                                       sliderInput(inputId = "x1Max", label = "X1 Max", min = 0, max = 100, value = 80, step = 5)
                               ),
                               
                               
                               div(
                                       id="divX2", style="display:inline-block",
                                       sliderInput(inputId = "x2Min", label = "X2 Min", min = 0, max = 100, value = 20, step = 5),
                                       sliderInput(inputId = "x2Max", label = "X2 Max", min = 0, max = 100, value = 80, step = 5)        
                               ),
                               
                               div(
                                       id="divReps",
                                       sliderInput(inputId = "myReps", label = "N Reps", min = 1, max = 5, value = 1)        
                               ),
                               
                               shinyjs::hidden(
                                       div(
                                               id="divGuess",
                                               numericInput(inputId = "x1Guess", "X1", value = NA),
                                               numericInput(inputId = "x2Guess", "X2", value = NA)
                                       )
                               ),
                               shinyjs::hidden(
                                       div(
                                               id="divSummary",
                                               
                                               valueBoxOutput("mySummary", width=12),    
                                               tags$br(), tags$br(),
                                               DT::dataTableOutput("dfSummary")
                                               
                                               
                                               
                                       )
                               ),
                               
                               shinyjs::hidden(
                                       div(
                                               id="divLinkOutput",
                                               tags$br(), tags$br(),
                                               actionLink(inputId = "linkOutput", label = "Show/Hide DOE")
                                       )
                               )
                       )
                )
                
        )
)
          

dashboardPage(myHeader, mySidebar, myBody)

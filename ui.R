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
                       
                       box(title = "Plot", width = 12,
                           plotlyOutput("plotResults")
                       ),
                       
                       box(title = "Data Table", width = 12, collapsible = TRUE, collapsed = TRUE,
                           DT::dataTableOutput("dfDOE")    
                       ),
                        
                       box(title = "Surface", width = 12, 
                           plotlyOutput("plotSurface")       
                       )
                       
                ),
                
                column(width=4,
                       
                       actionButton(inputId = "myUpdate", label = "Update"),
                       
                       selectInput(inputId = "myType", label = "Type", choices = c("Screening","Response Surface"), selected = "Screening"),
                       selectInput(inputId = "myBlocks",  label = "Blocks", choices = c("Corners"=1, "Star"=2), selected = c(1,2), multiple = TRUE),
                       sliderInput(inputId = "x1Min", label = "X1 Min", min = 0, max = 100, value = 20),
                       sliderInput(inputId = "x1Max", label = "X1 Max", min = 0, max = 100, value = 80),
                       sliderInput(inputId = "x2Min", label = "X2 Min", min = 0, max = 100, value = 20),
                       sliderInput(inputId = "x2Max", label = "X2 Max", min = 0, max = 100, value = 80),
                       sliderInput(inputId = "myReps", label = "N Reps", min = 1, max = 5, value = 1)
                )
                
        )
)
               
               # myCenter <- "No"
               # myBlocks <- c(1) # In Shiny Call "Corners" "Starts" but Return Numbers..
               
               
        




dashboardPage(myHeader, mySidebar, myBody)

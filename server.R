# Server

# Libraries

        library(shiny)
        library(shinydashboard)
        library(shinyjs)
        library(dplyr)
        library(ggplot2)
        library(plotly)
        library(rsm)
        library(DT)

# Constants
        
# Functions
        
        source("fScreen.R")
        source("fRSM.R")
        
# Server

shinyServer(function(input, output, session){
        
        myRV <- reactiveValues(
                myStart = TRUE,
                b0 = runif(n = 1, min = 1000, max = 6000),
                b1 = rnorm(n = 1, mean = 0, sd = 50),
                b2 = rnorm(n = 1, mean = 0, sd = 50),
                b12 = rnorm(n = 1, mean = 0, sd = 1),
                b11 = -abs(rnorm(n = 1, mean = 0, sd = 1.37)),
                b22 = -abs(rnorm(n = 1, mean = 0, sd = 1.37)),
                c1 = rnorm(n = 1, mean = 50, sd = 10),
                c2 = rnorm(n = 1, mean = 50, sd = 10),
                c3 = rnorm(n = 1, mean = 50, sd = 5),
                c4 = rnorm(n = 1, mean = 50, sd = 5),
                c5 = rnorm(n = 1, mean = 50, sd = 5),
                c6 = rnorm(n = 1, mean = 50, sd = 5),
                df1 = NULL,
                dfOpt = NULL
        )
        
        
        
        observe({
                # Create the Space   
                myRV$df1 <- tbl_df(data.frame(expand.grid(x1=0:100, x2=0:100))) %>%
                        mutate(z = myRV$b0 + myRV$b1*(x1-myRV$c1) + myRV$b2*(x2-myRV$c2) + myRV$b12*(x1-myRV$c3)*(x2-myRV$c4) + myRV$b11*(x1-myRV$c5)^2 + myRV$b22*(x2-myRV$c6)^2)
        })
        
        observe({
                # Find Optimal
                myRV$dfOpt <-  myRV$df1 %>%
                        filter(z == max(myRV$df1[["z"]]) | z == min(myRV$df1[["z"]])) %>%
                        arrange(desc(z))
        })
        
        observe({
                # Calculate Range of Response Variable
                myRV$myRange <- as.numeric(myRV$dfOpt[1,3] - myRV$dfOpt[2,3])
        })
        
        observe({
                # Assume Noise for Response Variable ~ 10% of Scale
                myRV$myS <- myRV$myRange * 0.10
        })
        
        observe({
                # Plot Total Response Surface
                plotSurface <- ggplot(data=myRV$df1, aes(x=x1, y=x2, z=z))
                plotSurface <- plotSurface + geom_contour(aes(col=..level..), bins=10, size=0.5)
                plotSurface <- plotSurface + geom_point(data = myRV$dfOpt[1,], aes(x=x1, y=x2), col=rgb(127,201,127, maxColorValue = 255), size=3)
                plotSurface <- plotSurface + geom_text(data=myRV$dfOpt[1,], aes(x=x1, y=x2), label="Optimal", col=rgb(127,201,127, maxColorValue = 255))
                plotSurface <- plotSurface + theme_minimal()
                plotSurface <- plotSurface + scale_color_continuous(low = rgb(166,206,227,maxColorValue = 255), high = rgb(0,51,102, maxColorValue = 255), name="Score")
                
                myRV$plotSurface <- plotSurface
                
        })
        
  
        session$onFlushed(function() {
                myRV$myStart <- TRUE
        })

        observeEvent(session$onFlushed, {
        
                # Re-initialize Data Frame

                dfDOE <- data.frame(x1 = numeric(), x2 = numeric(), z=numeric())   # This is a Null Data Frame to Which UI Designs are Added
                write.csv(x = dfDOE, file = "dfDOE.csv", row.names=FALSE)
           
                myRV$b0 <- runif(n = 1, min = 1000, max = 6000)
                myRV$b1 <- rnorm(n = 1, mean = 0, sd = 50)
                myRV$b2 <- rnorm(n = 1, mean = 0, sd = 50)
                myRV$b12 <- rnorm(n = 1, mean = 0, sd = 1)
                myRV$b11 <- -abs(rnorm(n = 1, mean = 0, sd = 1.37))
                myRV$b22 <- -abs(rnorm(n = 1, mean = 0, sd = 1.37))
                myRV$c1 <- rnorm(n = 1, mean = 50, sd = 10)
                myRV$c2 <- rnorm(n = 1, mean = 50, sd = 10)
                myRV$c3 <- rnorm(n = 1, mean = 50, sd = 5)
                myRV$c4 <- rnorm(n = 1, mean = 50, sd = 5)
                myRV$c5 <- rnorm(n = 1, mean = 50, sd = 5)
                myRV$c6 <- rnorm(n = 1, mean = 50, sd = 5)
              
        })
        
        observeEvent(input$myGuess, {
                
                shinyjs::hide(id="divMyUpdate", anim = TRUE, animType = "slide")
                shinyjs::hide(id="divType", anim = TRUE, animType = "slide")
                shinyjs::hide(id="divX1", anim = TRUE, animType = "slide")
                shinyjs::hide(id="divX2", anim = TRUE, animType = "slide")
                shinyjs::hide(id="divReps", anim = TRUE, animType = "slide")
                shinyjs::hide(id="divMyGuess", anim = TRUE, animType = "slide")
                shinyjs::show(id="divGuess", anim = TRUE, animType = "slide")
                shinyjs::show(id="divMyFinish", anim = TRUE, animType = "slide")
                
        })
        
        observeEvent(input$myFinish, {
                
                shinyjs::show(id="divPlotSurface", anim = TRUE, animType = "slide")
                shinyjs::hide(id="divGuess", anim = TRUE, animType = "slide")
                shinyjs::hide(id="divMyFinish", anim = TRUE, animType = "slide")
                shinyjs::show(id="divLinkOutput", anim = TRUE, animType = "slide")
                shinyjs::toggle(id = "divOutput", anim = TRUE, animType = "slide")
                shinyjs::show(id="divSummary", anim = TRUE, animType = "slide")

        })
        
        observeEvent(input$linkOutput,{
                shinyjs::toggle(id = "divOutput", anim = TRUE, animType = TRUE)
        })
        
        
        
        dfMyDesign <- reactive({  # This is the Current Design in the UI
                
                if(input$myType == "Screening"){
                        myDesign <- fScreen(input$x1Min, input$x1Max, input$x2Min, input$x2Max, input$myReps)
                } else if(input$myType == "Response Surface") {
                        myDesign <- fRSM(input$x1Min, input$x1Max, input$x2Min, input$x2Max, input$myReps, input$myBlocks)
                }
                
                myDesign <-  myDesign %>% 
                        mutate(z = myRV$b0 + myRV$b1*(x1-myRV$c1) + myRV$b2*(x2-myRV$c2) + myRV$b12*(x1-myRV$c3)*(x2-myRV$c4) + myRV$b11*(x1-myRV$c5)^2 + myRV$b22*(x2-myRV$c6)^2) %>% 
                        mutate(z = z + rnorm(n = nrow(myDesign), mean = 0, sd = myRV$myS))
                
                myDesign
                
        })
    
        dfDOE <- eventReactive(input$myUpdate, {
                
                dfDOE <- read.csv(file = "dfDOE.csv")
                dfDOE <- bind_rows(dfMyDesign(), dfDOE)
                
                write.csv(x = dfDOE, file = "dfDOE.csv", row.names = FALSE)
                
                dfDOE
                
                })

       
        output$dfDOE <- DT::renderDataTable({
                
                dfDOE()
                
        })
        
        mModelCoeff <- reactive({
                
                # Model
                
                lmOut <- summary(lm(data=dfDOE(), z ~ x1 + x2 + x1*x2 + I(x1^2) + I(x2^2)))
                
                # Extract Coefficients
                
                myCoeff <- lmOut$coefficients[ , 1]
                
                b0_ <- myCoeff["(Intercept)"]
                b1_ <- myCoeff["x1"]
                b2_ <- myCoeff["x2"]
                b12_ <- myCoeff["x1:x2"]
                b11_ <- myCoeff["I(x1^2)"]
                b22_ <- myCoeff["I(x2^2)"]
                
                if(is.na(b11_)){b11_ <- 0}
                if(is.na(b22_)){b22_ <- 0}
                
                mModelCoeff <- matrix(c(b0_, b1_, b2_, b12_, b11_, b22_))
                mModelCoeff
                
        })
        
        
        dfOut <- reactive({
                
                dfDOE <- dfDOE()
                mModelCoeff <- mModelCoeff()
                
                b0_ <- mModelCoeff[1,1]
                b1_ <- mModelCoeff[2,1]
                b2_ <- mModelCoeff[3,1]
                b12_ <- mModelCoeff[4,1]
                b11_ <- mModelCoeff[5,1]
                b22_ <- mModelCoeff[6,1]
                
                dfOut <- expand.grid(x1=min(dfDOE$x1):max(dfDOE$x1),x2=min(dfDOE$x2):max(dfDOE$x2))
                dfOut <- dfOut %>% 
                        mutate(z = b0_ + b1_*x1 + b2_*x2 + b12_*x1*x2 + b11_*x1^2 + b22_*x2^2)
                
                dfOut
                
        })
        
        output$plotResults <- renderPlotly({

                dfMyDesign <- dfMyDesign()

                p1 <- ggplot(data=dfMyDesign, aes(x=x1, y=x2))
                p1 <- p1 + geom_point(data=dfMyDesign, aes(x=x1, y=x2), size = 3, col=rgb(190,174,212, maxColorValue = 255))

                if(input$myUpdate > 0){
                        
                        dfDOE <- dfDOE()
                        dfOut <- dfOut()
                        
                        # p1 <- p1 + (data=dfDOE, aes(x=x1, y=x2, label=round(z,1)))
                        p1 <-p1 + geom_point(data=dfDOE, size=3, shape = 10, col=rgb(127,201,127, maxColorValue = 255))
                        p1 <- p1 + geom_contour(data=dfOut, aes(x=x1, y=x2, z=z, col=..level..), size=0.5)
                        p1 <- p1 + scale_color_continuous(low = rgb(166,206,227,maxColorValue = 255), high = rgb(0,51,102, maxColorValue = 255), name="Score")
                        
                }
                
                p1 <- p1 + theme_minimal()
                p1 <- p1 + scale_x_continuous(limits=c(0,100))
                p1 <- p1 + scale_y_continuous(limits=c(0,100))

                p1 <- plotly_build(p1)

                p1

        })
        
        output$plotSurface <- renderPlotly({
                
                x1Guess <- round(input$x1Guess,0)
                x2Guess <- round(input$x2Guess,0)
                
                df2 <- myRV$df1 %>% 
                        filter(x1 == x1Guess, x2 == x2Guess) %>% 
                        mutate(z = myRV$b0 + myRV$b1*(x1-myRV$c1) + myRV$b2*(x2-myRV$c2) + myRV$b12*(x1-myRV$c3)*(x2-myRV$c4) + myRV$b11*(x1-myRV$c5)^2 + myRV$b22*(x2-myRV$c6)^2)
                
                plotSurface <- myRV$plotSurface + geom_point(data = df2, aes(x=x1, y=x2), col=rgb(red = 217, green = 95, blue = 2, maxColorValue = 255))
                plotSurface <- plotSurface + geom_text(data = df2, aes(x=x1, y=x2), label="Guess", col=rgb(red = 217, green = 95, blue = 2, maxColorValue = 255))
                
                p <- plotly_build(plotSurface)
                p
                
        })
        
        
        dfSummary <- reactive({
                
                req(input$x1Guess, input$x2Guess)
                
                x1Guess <- round(input$x1Guess,0)
                x2Guess <- round(input$x2Guess,0)
                
                dfMax <- myRV$dfOpt %>% select(x1, x2, z) %>% filter(row_number() == 1)
                
                dfGuess <- myRV$df1 %>% 
                        filter(x1 == x1Guess, x2 == x2Guess) %>% 
                        mutate(z = myRV$b0 + myRV$b1*(x1-myRV$c1) + myRV$b2*(x2-myRV$c2) + myRV$b12*(x1-myRV$c3)*(x2-myRV$c4) + myRV$b11*(x1-myRV$c5)^2 + myRV$b22*(x2-myRV$c6)^2)
                
                dfSummary <- bind_rows(dfMax, dfGuess)
                dfSummary$Score <- round(dfSummary$z,0)
                dfSummary$Source <- c("Optimal", "Guess")
                dfSummary <- dfSummary %>% select(-z)
                
                dfSummary
                
        })
        
        
        output$dfSummary <- DT::renderDataTable({

                dfSummary()
                
        }, options=list(dom="t"))
        
        
        output$mySummary <- renderValueBox({
                
                myDiff <- round(dfSummary()[1,3] - dfSummary()[2,3],1)
                
                myPercentDiff <- round(100*(dfSummary()[1,3] - dfSummary()[2,3])/dfSummary()[1,3],1)
                
                if(myPercentDiff < 10){
                        valueBox(
                                subtitle = "% Difference", width = 12,
                                value = myPercentDiff, 
                                color = "green", icon = icon("thumbs-up")
                        )       
                } else if(myPercentDiff < 20){
                        valueBox(
                                
                                subtitle = "% Difference", width = 12,
                                value = myPercentDiff, 
                                color = "yellow", icon = icon("exclamation")
                        )
                } else (
                        valueBox(
                                subtitle = "% Difference", width = 12,
                                value = myPercentDiff, 
                                color = "red", icon = icon("thumbs-down")
                        )
                )
                
        })
        
        
        
})
        
        
        
        
        
        
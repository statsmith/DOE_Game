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
        
        # Define the Relationship Between Response (y) and Factors (x1, x2)

        b0 <- runif(n = 1, min = 1000, max = 6000)
        b1 <- rnorm(n = 1, mean = 0, sd = 50)
        b2 <- rnorm(n = 1, mean = 0, sd = 50)
        b12 <- rnorm(n = 1, mean = 0, sd = 1)
        b11 <- -abs(rnorm(n = 1, mean = 0, sd = 1.37))
        b22 <- -abs(rnorm(n = 1, mean = 0, sd = 1.37))
        c1 <- rnorm(n = 1, mean = 50, sd = 10)
        c2 <- rnorm(n = 1, mean = 50, sd = 10)
        c3 <- rnorm(n = 1, mean = 50, sd = 5)
        c4 <- rnorm(n = 1, mean = 50, sd = 5)
        c5 <- rnorm(n = 1, mean = 50, sd = 5)
        c6 <- rnorm(n = 1, mean = 50, sd = 5)
        
        # 
        # b0 <- runif(n = 1, min = 4, max = 60)
        # b1 <- rnorm(n = 1, mean = 0, sd = 5)
        # b2 <- rnorm(n = 1, mean = 0, sd = 5)
        # b12 <- rnorm(n = 1, mean = 0, sd = 0.05)
        # b11 <- -abs(rnorm(n = 1, mean = 0, sd = 0.05))
        # b22 <- -abs(rnorm(n = 1, mean = 0, sd = 0.05))
        
        # Initialize Data Frame
        
        dfDOE <- data.frame(x1 = numeric(), x2 = numeric(), z=numeric())   # This is a Null Data Frame to Which UI Designs are Added
        write.csv(x = dfDOE, file = "dfDOE.csv", row.names=FALSE)
        
        # Create the Space
        
        df1 <- tbl_df(data.frame(expand.grid(x1=0:100, x2=0:100)))
        df1 <- df1 %>% 
                mutate(z = b0 + b1*(x1-c1) + b2*(x2-c2) + b12*(x1-c3)*(x2-c4) + b11*(x1-c5)^2 + b22*(x2-c6)^2)
                # mutate(z = b0 + b1*(x1-50) + b2*(x2-50) + b12*(x1-50)*(x2-50) + b11*(x1-50)^2 + b22*(x2-50)^2)
        
        # Find Optimal
        
        dfOpt <- df1 %>% 
                filter(z == max(df1$z) | z == min(df1$z)) %>% 
                arrange(desc(z))
        
        # Calculate Range of Response Variable
        
        myRange <- as.numeric(dfOpt[1,3] - dfOpt[2,3])
        
        # Assume Noise for Response Variable ~ 10% of Scale
        
        myS <- myRange * 0.10
        
        # Plot Total Response Surface
        
        plotSurface <- ggplot(data=df1, aes(x=x1, y=x2, z=z))
        plotSurface <- plotSurface + geom_contour(aes(col=..level..), bins=10, size=0.5)
        plotSurface <- plotSurface + geom_point(data = dfOpt[1,], aes(x=x1, y=x2), col=rgb(127,201,127, maxColorValue = 255), size=3)
        plotSurface <- plotSurface + geom_text(data=dfOpt[1,], aes(x=x1, y=x2), label="Optimal", col=rgb(127,201,127, maxColorValue = 255))
        plotSurface <- plotSurface + theme_minimal()
        plotSurface <- plotSurface + scale_color_continuous(low = rgb(166,206,227,maxColorValue = 255), high = rgb(0,51,102, maxColorValue = 255), name="Score")
        
        
        plotSurface
      
# Functions
        
        source("fScreen.R")
        source("fRSM.R")
        
# Server

shinyServer(function(input, output, session){
        
        myRV <- reactiveValues(myStart = TRUE)
        
        session$onFlushed(function() {
                myRV$myStart <- FALSE
        })
        
        observeEvent(session$onFlushed, {
                dfDOE <- data.frame(x1=numeric, x2=numeric, Block=numeric, z=numeric)
                write.csv(x = dfDOE, file = "dfDOE.csv",row.names = FALSE)
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
                        mutate(z = b0 + b1*(x1-c1) + b2*(x2-c2) + b12*(x1-c3)*(x2-c4) + b11*(x1-c5)^2 + b22*(x2-c6)^2) %>% 
                        # mutate(z = b0 + b1*(x1-50) + b2*(x2-50) + b12*(x1-50)*(x2-50) + b11*(x1-50)^2 + b22*(x2-50)^2) %>% 
                        mutate(z = z + rnorm(n = nrow(myDesign), mean = 0, sd = myS))
                
                # print(myDesign)
                
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
                
                df2 <- df1 %>% 
                        filter(x1 == input$x1Guess, x2 == input$x2Guess) %>% 
                        mutate(z = b0 + b1*(x1-c1) + b2*(x2-c2) + b12*(x1-c3)*(x2-c4) + b11*(x1-c5)^2 + b22*(x2-c6)^2)
                        # mutate(z = b0 + b1*(x1-50) + b2*(x2-50) + b12*(x1-50)*(x2-50) + b11*(x1-50)^2 + b22*(x2-50)^2)
                
                plotSurface <- plotSurface + geom_point(data = df2, aes(x=x1, y=x2), col=rgb(red = 217, green = 95, blue = 2, maxColorValue = 255))
                plotSurface <- plotSurface + geom_text(data = df2, aes(x=x1, y=x2), label="Guess", col=rgb(red = 217, green = 95, blue = 2, maxColorValue = 255))
                
                p <- plotly_build(plotSurface)
                p
                
        })
        
        
        
        dfSummary <- reactive({
                
                req(input$x1Guess, input$x2Guess)
                
                dfMax <- dfOpt %>% select(x1, x2, z) %>% filter(row_number() == 1)
                
                dfGuess <- df1 %>% 
                        filter(x1 == input$x1Guess, x2 == input$x2Guess) %>% 
                        mutate(z = b0 + b1*(x1-c1) + b2*(x2-c2) + b12*(x1-c3)*(x2-c4) + b11*(x1-c5)^2 + b22*(x2-c6)^2)
                
                dfSummary <- bind_rows(dfMax, dfGuess)
                dfSummary$Score <- round(dfSummary$z,0)
                dfSummary$Source <- c("Optimal", "Guess")
                dfSummary <- dfSummary %>% select(-z)
                
                dfSummary
                
        })
        
        
        output$dfSummary <- DT::renderDataTable({
                
                dfSummary()
                
        }, options=list(dom="t"))
        
        
        # output$mySummary <- renderText({
        #         
        #         myDiff <- round(dfSummary()[1,3] - dfSummary()[2,3],1)
        #         myPercentDiff <- round(100*(dfSummary()[1,3] - dfSummary()[2,3])/dfSummary()[1,3],1)
        #         # print(myPercentDiff)
        #         # print(nrow(dfDOE()))
        #         mySummary <- paste("You used ", nrow(dfDOE()), " runs and were ", myPercentDiff, "% (",  myDiff, " units) off from the optimal", sep="")
        #         
        # })
        
        output$mySummary <- renderValueBox({
                
                myDiff <- round(dfSummary()[1,3] - dfSummary()[2,3],1)
                myPercentDiff <- round(100*(dfSummary()[1,3] - dfSummary()[2,3])/dfSummary()[1,3],1)
                mySummary <- paste("You used ", nrow(dfDOE()), " runs and were ", myPercentDiff, "% (",  myDiff, " units) off from the optimal", sep="")
                
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
        
        
        
        
        
        
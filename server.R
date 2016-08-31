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
        
        b0 <- runif(n = 1, min = 4, max = 60)
        b1 <- rnorm(n = 1, mean = 0, sd = 5)
        b2 <- rnorm(n = 1, mean = 0, sd = 5)
        b12 <- rnorm(n = 1, mean = 0, sd = 0.05)
        b11 <- -abs(rnorm(n = 1, mean = 0, sd = 0.05))
        b22 <- -abs(rnorm(n = 1, mean = 0, sd = 0.05))
        
        # Initialize Data Frame
        
        dfDOE <- data.frame(x1 = numeric(), x2 = numeric(), z=numeric())   # This is a Null Data Frame to Which UI Designs are Added
        write.csv(x = dfDOE, file = "dfDOE.csv", row.names=FALSE)
        
        # Create the Space
        
        df1 <- tbl_df(data.frame(expand.grid(x1=0:100, x2=0:100)))
        df1 <- df1 %>% 
                mutate(z = b0 + b1*(x1-50) + b2*(x2-50) + b12*(x1-50)*(x2-50) + b11*(x1-50)^2 + b22*(x2-50)^2)
        
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
        plotSurface <- plotSurface + geom_contour(aes(col=..level..), bins=10)
        plotSurface <- plotSurface + geom_point(data = dfOpt[1,], aes(x=x1, y=x2), col=rgb(127,201,127, maxColorValue = 255), size=3)
        plotSurface <- plotSurface + theme_minimal()
        plotSurface
      
# Functions
        
        source("fScreen.R")
        source("fRSM.R")
        
# Server

shinyServer(function(input, output, session){
        
        observeEvent(input$myGuess, {
                
                shinyjs::hide(id="divMyUpdate", anim = TRUE, animType = "slide")
                shinyjs::hide(id="divType", anim = TRUE, animType = "slide")
                shinyjs::hide(id="divX1", anim = TRUE, animType = "slide")
                shinyjs::hide(id="divX2", anim = TRUE, animType = "slide")
                shinyjs::hide(id="divReps", anim = TRUE, animType = "slide")
                shinyjs::show(id="divGuess", anim = TRUE, animType = "slide")
                shinyjs::show(id="divMyFinish", anim = TRUE, animType = "slide")
                
        })
        
        observeEvent(input$myFinish, {
                
                shinyjs::show(id="divPlotSurface", anim = TRUE, animType = "slide")
                shinyjs::hide(id="divGuess", anim = TRUE, animType = "slide")
                
        })
        
        
        
        dfMyDesign <- reactive({  # This is the Current Design in the UI
                
                if(input$myType == "Screening"){
                        myDesign <- fScreen(input$x1Min, input$x1Max, input$x2Min, input$x2Max, input$myReps)
                } else if(input$myType == "Response Surface") {
                        myDesign <- fRSM(input$x1Min, input$x1Max, input$x2Min, input$x2Max, input$myReps, input$myBlocks)
                }
                
                myDesign <-  myDesign %>% 
                        mutate(z = b0 + b1*(x1-50) + b2*(x2-50) + b12*(x1-50)*(x2-50) + b11*(x1-50)^2 + b22*(x2-50)^2) %>% 
                        mutate(z = z + rnorm(n = nrow(myDesign), mean = 0, sd = myS))
                
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
                        p1 <- p1 + geom_contour(data=dfOut, aes(x=x1, y=x2, z=z, col=..level..))
                        
                }
                
                p1 <- p1 + theme_minimal()
                p1 <- p1 + scale_x_continuous(limits=c(0,100))
                p1 <- p1 + scale_y_continuous(limits=c(0,100))

                p1 <- plotly_build(p1)

                p1

        })
        
        output$plotSurface <- renderPlotly({
                
                print(df1)
                df2 <- df1 %>% 
                        filter(x1 == input$x1Guess, x2 == input$x2Guess) %>% 
                        mutate(z = b0 + b1*(x1-50) + b2*(x2-50) + b12*(x1-50)*(x2-50) + b11*(x1-50)^2 + b22*(x2-50)^2)
                
                plotSurface <- plotSurface + geom_point(data = df2, aes(x=x1, y=x2))
                
                p <- plotly_build(plotSurface)
                p
                
        })
        
})
        
        
        
        
        
        
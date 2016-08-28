library(rsm)
library(dplyr)
library(ggplot2)

 
# x1Min <- 20
# x1Max <- 50
# x2Min <- 30
# x2Max <- 50
# myReps <- 2

fRSM <- function(x1Min, x1Max, x2Min, x2Max, myReps, myBlocks){
        
        dfDOE <- ccd(2, n0 = c(1,1), inscribed=TRUE, randomize=FALSE)
        dfDOE <- data.frame(x1=dfDOE$x1, x2=dfDOE$x2, Block=dfDOE$Block)

        dfDOE$x1 <- dfDOE$x1*((x1Min + x1Max)/2 - x1Min) + (x1Min + x1Max)/2
        dfDOE$x2 <- dfDOE$x2*((x2Min + x2Max)/2 - x2Min) + (x2Min + x2Max)/2
        
        
        if(myReps > 1){
                
                dfDOETemp <- dfDOE
                
                for(i in 1:(myReps-1)){
                        dfDOE <- bind_rows(dfDOE, dfDOETemp)
                }
        }
        
        dfDOE <- dfDOE %>% filter(Block %in% myBlocks)
        dfDOE$Block <- as.numeric(dfDOE$Block)
        dfDOE
        
        
        
        
}
        
# 
# dfDOE <-  fRSM(10, 70, 200, 30, 2, c(2))
# p <- ggplot(data=dfDOE, aes(x=x1, y=x2, col=Block, shape= Block))
# p <- p + geom_point(size=8)
# p <- p + theme_minimal()
# p
# 
#    
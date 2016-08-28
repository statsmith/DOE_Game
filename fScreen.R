fScreen <- function(x1Min, x1Max, x2Min, x2Max, myReps, myCenter="No"){
        
        # Not Planning to Include Center Points Yet, But For Future...
        
        if(myCenter=="Yes"){
                x1Center <- mean(c(x1Min, x1Max))
                x2Center <- mean(c(x2Min, x2Max))
        }
        
        
        # Design
        
        dfDOE <- expand.grid(x1 = c(x1Min, x1Max), x2 = c(x2Min, x2Max))
        
        # Not Planning to Include Center Points Yet, But For Future...
        if(myCenter=="Yes"){
                dfDOE <- rbind(dfDOE, c(x1=x1Center, x2Center))        
        }
        
        if(myReps > 1){
                
                dfDOETemp <- dfDOE
                
                for(i in 1:(myReps-1)){
                        dfDOE <- bind_rows(dfDOE, dfDOETemp)
                }
        }

        dfDOE$Block <- NA
        dfDOE
        
        
}
        

# fScreen(-60,60,-60,60,5)
        
   
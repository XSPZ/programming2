best <- function(state, outcome) {
          hosname <- vector(mode = "character")
           data <- read.csv("outcome-of-care-measures.csv", header = TRUE)
           dat <- data.frame(data)
            ##attach(dat)
           
           list <- dat[State == state,]
           if(!is.element(state, data$State)) {
             stop("invalid state")
           }
           hattack <- list$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
           hfailure <- list$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
           pneumonia <- list$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
           name <- list$Hospital.Name
           
              if(outcome == "heart attack") {
                minLoc <- which.min(hattack)
                 
              } else if(outcome == "heart failure") {
                     minLoc <- which.min(hfailure) 
                    
                
               
              } else if(outcome == "pneumonia") {
                      minLoc <- which.min(as.vector(pneumonia, mode = "any"))
                    
            
              }
            
       
           
                  
          hosname <- name[minLoc]
        
          hos <- as.character(hosname)
          return(hos)
}

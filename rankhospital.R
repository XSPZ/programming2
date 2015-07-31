rankhospital <- function(state, outcome, num = "best") {
      data <- read.csv("outcome-of-care-measures.csv", header = TRUE)
      dat <- data.frame(data)
      attach(dat)
      list <- dat[State == state,]
      if(!is.element(state, data$State)) {
        stop("invalid state")
      }
      
      if(!is.element(outcome, "heart attack") && !is.element(outcome, "heart failure")
                      && !is.element(outcome, "pneumonia")) {
        stop("invalid outcome")
      }
      
      hattack <- list$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
      hfailure <- list$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
      pneumonia <- list$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
      name <- list$Hospital.Name
      
      if(outcome == "heart attack") {
          new <- data.frame(name, hattack)
          newdata <- new[order(hattack, name),]
        
      } else if(outcome == "heart failure") {
          new <- data.frame(name, hfailure)
          newdata <- new[order(hfailure, name),]
      } else if(outcome == "pneumonia") {
          new <- data.frame(name, pneumonia)
          newdata <- new[order(pneumonia, name),]
      }
      
      for(i in 1:length(list)) {
          if(num == i) {
              rank <- as.vector(newdata[i,1], mode = "any")
          } 
          
        } 
      
      ##if(num > length(list)) {
              ##rank <- NA
          ##} 
      if(num == "best") {
              rank <- as.vector(newdata[1,1], mode = "any")
          } else if(num == "worst") {
              for(j in 1:length(newdata)) {
                  if(newdata[j,2] == "Not Available") {
                    remove(newdata[j,])
                  }
              ##good <- complete.cases(newdata)
              ##newdata2 <- newdata[good]
              rank <- as.vector(newdata[length(newdata),1], mode = "any")
          }
        }
      
      print(rank)
}

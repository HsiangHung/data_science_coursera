best <- function(state,outcome){
      
      ## Read outcome data
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      statename <- data$State 
      
      if (state %in% statename == FALSE) {
            stop('invalid state')
      }
      
      if (outcome == 'heart attack'){
            x <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
      } else if (outcome == 'heart failure') {
            x <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
      } else if (outcome == 'pneumonia') {
            x <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
      } else {
            stop('invalid outcome')
      }
      
      Y <- split(data[x],data$State)
      name <- split(data$Hospital.Name,data$State)
      hospital_name <- name[[state]]
      
      k <- as.numeric(unlist(Y[state]))
      min_numb = min(k,na.rm=TRUE)
      min_posit = which.min(k)
      
      ## Check that state and outcome are valid
      print(state)
      ## Return hospital name in that state with lowest 30-day death
      print(hospital_name[min_posit])
      ## rate
      print(min_numb)
}


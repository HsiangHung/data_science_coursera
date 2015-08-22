best <- function(state,outcome){
      print(state)
      ## Read outcome data
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      if (outcome == 'heart attack'){
            x <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
      } else if (outcome == 'heart failure') {
            x <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
      } else if (outcome == 'pneumonia') {
            x <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
      } else {
            return #print('invalid outcome')
      }
      #a <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
      #f <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
      #p <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
      #Ya <- split(data[a],data$State)
      #Yf <- split(data[f],data$State)
      #Yp <- split(data[p],data$State)
      
      Y <- split(data[x],data$State)
      
      #Yp <- split(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,outcome$State)
      #Yf <- split(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,outcome$State)
      #Ya <- split(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,outcome$State)
      
      name <- split(data$Hospital.Name,data$State)
      hospital_name <- name[[state]]
      
      k <- as.numeric(unlist(Y[state]))
      min_numb = min(k,na.rm=TRUE)
      min_posit = which.min(k)
      
      #print(min(k,na.rm=TRUE))
      #print(t$"TX")
      #print(t$MD)
      #print(name[state])
      print(min_numb)
      #print(hospital_name[(1)])
      print(k)
      print(which.min(k))
      #print(split(data$Hospital.Name,data$State)$state)
      
      #print(hospital_name$MD[min_posit])
      
      print(hospital_name[min_posit])
      
      #print (list(hospital_name[which.min(k)]))
      ## Check that state and outcome are valid
      ## Return hospital name in that state with lowest 30-day death
      ## rate
}


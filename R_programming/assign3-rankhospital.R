rankhospital <- function(state,outcome,num){
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
      #min_numb = min(k,na.rm=TRUE)
      #min_posit = which.min(k)
      
      sample_numb <- length(hospital_name)
      #print(sample_numb)
      
      rank <- order(k,na.last =TRUE)
      raterank <- k[rank[1:sample_numb]]
      b <- is.na(k)
      last_rank <- length(b[b==FALSE])
      namerank <- hospital_name[rank[1:sample_numb]]
      
      #print(data.frame(Hospital.Name=namerank,Rate=raterank,Rank=1:sample_numb))
      
      if (num =='best'){
            num =1
      } else if (num == 'worst') {
            num =last_rank
      }
      #print(num)
      #print(data.frame(Hospital.Name=namerank[1:num],Rate=raterank[1:num],Rank=1:num))
      
      ## Check that state and outcome are valid
      #print(state)
      ## Return hospital name in that state with lowest 30-day death
      ## 30-day death rate
      result <- namerank[num]
      return(result)
}


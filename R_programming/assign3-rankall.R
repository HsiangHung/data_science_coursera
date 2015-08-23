rankall <- function(outcome,num='best'){
      ## Read outcome data
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
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
      name  <- split(data$Hospital.Name,data$State)
      name2 <- split(data$State,data$State)
      
      numb_state <- length(as.character(name))  ## this counts how many different states (group)
      #print(numb_state)   
      
      hospital <- c( )
      state_name <- c( )
      for (state in 1 : numb_state){
      #for (state in 1 : 10){
                  
            hospital_name <- name[[state]]
            name3 <- name2[[state]]
            state_name[state] <- name3[1]
            
            #print(name3[1])
            
            sample_numb <- length(hospital_name)    
            k <- as.numeric(unlist(Y[state]))       # to convert the characters from table to numbers
            #print(k)

            rank <- order(k,na.last =TRUE)          # this gives position of k with rate in ascending order.
            raterank <- k[rank[1:sample_numb]]      # this rearrange a vector of k with rates in ascending order.
            b <- is.na(k)
            last_rank <- length(b[b==FALSE])        # this gives the number of non-NA elements of rearranged vector.
            namerank <- hospital_name[rank[1:sample_numb]]  # this gives a vector of hospital names with rate...
            #print(data.frame(Hospital.Name=namerank,Rate=raterank,Rank=1:sample_numb))
            #print(last_rank)
            
            # the following logical is to determine, best, last, or certain rank
            if (num =='best'){
                  rankth =1
            } else if (num == 'worst') {
                  rankth =last_rank
            } else {
                  rankth = num               
            }
            #print(rankth)
            #print(data.frame(Hospital.Name=namerank[1:rankth],Rate=raterank[1:rankth],Rank=1:rankth))
            hospital[state] <- namerank[rankth]
            #write(hospital[state],state_name[state])
      }
      result <- data.frame(state=state_name,hospital=hospital)#,check.rows = FALSE)
      return(result)
}


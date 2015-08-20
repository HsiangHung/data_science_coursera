complete <- function(directory, id=1:332) {
      complete <- c( )
      for (i in seq_along(id) ) {
            listcsv <- dir(pattern = "*.csv")
            name = id[i]
            data <- read.csv(listcsv[name])
            #data <- read.csv('010.csv')
            #print(nrow(data))
            a1 <- data[,2]
            a2 <- is.na(a1)
            #print(table(a2))
            #print(a2)
            sample_sulfate <- length(a2[a2==FALSE])
            b1 <- data[,3]
            b2 <- is.na(b1)
            #print(table(b2))
            #print(b2)
            #print(data)
            complete[i] <- 0
            for (j in 1:nrow(data)){
                  if ((a2[j] ==0) && (b2[j]==0)){ 
                     complete[i] <- complete[i] + 1
                  }
            }
      }
      result <- data.frame(id = id, nobs = complete)
      return(result)
}      

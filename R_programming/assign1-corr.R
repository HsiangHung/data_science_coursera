corr <- function(directory, threshold=0) {
      correlation <- 0
      crr <- c( )
      for (i in 1:332) {
            number <- complete('~/Users/hhhung/Desktop/specdata/',i)
            #print(number[[2]])
            if (number[[2]] > threshold ){
                  listcsv <- dir(pattern = "*.csv")
                  data <- read.csv(listcsv[i])
                  sulfate <- is.na(data[,2])
                  nitrate <- is.na(data[,3])
  
                  complete_sample <- 0
                  s <- c( )
                  n <- c( )
                  for (j in 1:nrow(data)){
                        if ((sulfate[j] == 0) && (nitrate[j]== 0)) {
                              complete_sample <- complete_sample+1
                              s[complete_sample] <- data[j,2]
                              n[complete_sample] <- data[j,3]
                        }
                  }
                  #result <- c(int(i),cor(s,n))
                  #print(result)
                  correlation <- correlation + 1
                  crr[correlation] <- cor(s,n)
                  #print(cor(s,n))
            }
      }
      return(crr)
}      

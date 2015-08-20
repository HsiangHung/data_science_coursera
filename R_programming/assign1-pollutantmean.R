pollutantmean <- function(directory, pollutant, id=1:332) {
      setwd("/Users/hhhung/Desktop/specdata")
      #print(id)
      #print(seq_along(id))
      mean <- 0
      number_sample <- 0
      for (i in seq_along(id) ) {
      #for (i in 1:332){
            listcsv <- dir(pattern = "*.csv")
            name = id[i]
            #name = i
            data <- read.csv(listcsv[name])
            #print(nrow(data))
            #data <- read.csv('010.csv')
            #print(listcsv[name])
            if (pollutant == "sulfate") {
                  col <- 2
            }else if (pollutant == "nitrate") {
                 col <- 3
            }
            #print(col)
            #print(data)
            a <- data[,col]
            b <- is.na(a)
            #print(table(b))
            #print(b)
            sample <- length(b[b==FALSE])
            #print(sample)
            if (sample != 0) {
                  mean <- mean +  mean(a[!b])*sample
            } 
            number_sample <- number_sample + sample
            #print(sample)
            #print(mean(a[!b]))
      }
      pollutantmean <- mean/number_sample
      print(pollutantmean)
}      

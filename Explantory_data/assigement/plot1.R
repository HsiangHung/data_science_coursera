data <- read.table("household_power_consumption.txt",header=TRUE,sep=";")
z <- data$Date
z1 <- as.Date(z,"%d/%m/%Y")
data2 <- cbind(data,z1)
# now data2 is a nrow * 10 data, the last column is z1, converting 
# "day/month/year" to "year-month-day"
# we only need 2007-02-01 and 2007-02-02 two days:
x1.sub <- subset(data2, z1 =="2007-02-01")
x2.sub <- subset(data2, z1 =="2007-02-02")
# check the dimension of x1.sub:
dim(x1.sub)
dim(x2.sub)
# combine as a data:
data3 <- rbind(x1.sub,x2.sub)
# the above statements are euqal to
# data3 <- subset(data2, z1=="2007-02-01" | z1=="2007-02-02")

# check dimension again:
dim(data3)
# also check our combination and selection are correct:
head(data3)
tail(data3)
# now take plot 1:
x <- as.numeric(data3$Global_active_power)
hist(x,col="red",xlab='Global Active Power',main="Global Active Power")
dev.copy(png,file="plot1.png")
dev.off()
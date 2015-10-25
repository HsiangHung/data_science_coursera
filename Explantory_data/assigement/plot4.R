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
# now take plot 4 and preparing all data:

par(mfrow=c(2,2))

x <- 1:2880


v <- data3$Global_active_power
plot(x,v,pch =".",ylab="Global Active Power",xlab="time")
lines(x,v)

################################################
w <- data3$Voltage
plot(x,w,pch =".",ylab="Voltage",xlab="time")
lines(x,w)

################################################
y1 <- data3$Sub_metering_1
y2 <- data3$Sub_metering_2
y3 <- data3$Sub_metering_3
plot(x,y1,pch =".",ylab="Energy sub metering",xlab="time",mar=c(2,2,2,2),col='black')
lines(x,y1)
points(x,y2,pch=".",col="red")
lines(x,y2,col='red')
points(x,y3,pch=".",col="blue")
lines(x,y3,col='blue')
legend("topright",lty=3,col=c("black","red","blue"),c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))

##################################################

z <- data3$Global_reactive_power
plot(x,z,pch =".",ylab="Global Reactive Power",xlab="time")
lines(x,z)

##################################################
dev.copy(png,file="plot4.png")
dev.off()
Fdata<- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))

## Change the Date Format. We need to do this for Plot Number 2.
Fdata$Date <- as.Date(Fdata$Date, "%d/%m/%Y")
  
## Subset the dataset (Data from Feb. 1, 2007 to Feb. 2, 2007)
Fdata <- subset(Fdata,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))
  
## Remove incomplete observation
Fdata <- Fdata[complete.cases(Fdata),]

## Combine Date and Time column (WE need to do it for having Accurate Plot)
DateTime <- paste(Fdata$Date, Fdata$Time)
  
## Name the vector
DateTime <- setNames(DateTime, "DateTime")
  
## Remove Unnecessary Information.  Date and Time columns are not necessary anymore
Fdata <- Fdata[ ,!(names(Fdata) %in% c("Date","Time"))]
  
## Add DateTime column
Fdata <- cbind(DateTime, Fdata)
  
## Format dateTime Column
Fdata$DateTime <- as.POSIXct(DateTime)

#Plot1
hist(Fdata$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col="red")

#Save Plot1.png and close device
dev.copy(png,"plot1.png", width=480, height=480)
dev.off()

#Plot2
plot(Fdata$Global_active_power~Fdata$DateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")

#Save Plot1.png and close device
dev.copy(png,"plot2.png", width=480, height=480)
dev.off()

# Plot3
with(Fdata, {
    plot(Sub_metering_1~DateTime, type="l",
         ylab="Global Active Power (kilowatts)", xlab="")
    lines(Sub_metering_2~DateTime,col='Red')
    lines(Sub_metering_3~DateTime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

 #Save Plot3.png and close device
 dev.copy(png, file="plot3.png", height=480, width=480)
 dev.off()
 
 #Plot4
 par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
 with(Fdata, {
     plot(Global_active_power~DateTime, type="l", 
          ylab="Global Active Power (kilowatts)", xlab="")
     plot(Voltage~DateTime, type="l", 
          ylab="Voltage (volt)", xlab="")
     plot(Sub_metering_1~DateTime, type="l", 
          ylab="Global Active Power (kilowatts)", xlab="")
     lines(Sub_metering_2~DateTime,col='Red')
     lines(Sub_metering_3~DateTime,col='Blue')
     legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
            legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
     plot(Global_reactive_power~DateTime, type="l", 
          ylab="Global Rective Power (kilowatts)",xlab="")
 })
 
 #Save Plot4.png and close device
   dev.copy(png, file="plot4.png", height=480, width=480)
   dev.off()

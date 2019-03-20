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

#Plot2
plot(Fdata$Global_active_power~Fdata$DateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")

#Save Plot1.png and close device
dev.copy(png,"plot2.png", width=480, height=480)
dev.off()

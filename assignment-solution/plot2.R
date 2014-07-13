readData <- function(filename) {
  setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%Y"))
  
  colNames <- c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", 
                "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
  
  
  # First we will just read the first column from data and find our data boundaries
  # to use later when we read the full data.
  classes <- c("myDate", rep("NULL", times=8)) # read only first column
  data <- read.table(filename, header=TRUE, sep=";", na.strings="?", colClasses=classes)
  
  min_row <- which.max(data$Date == as.Date("2007-02-01")) #first row to read
  max_row <- which.max(data$Date > as.Date("2007-02-02")) #first row to not read
  
  skip <- min_row ## number of rows to skip including header 
  nrows <- max_row - min_row 
  
  classes <- c("myDate","character",rep("numeric", times=7))
  data <- read.table(filename, header=FALSE, sep=";", na.strings="?", colClasses=classes, col.names=colNames, skip=skip, nrows=nrows)
  data$datetime = strptime(paste(data$Date, data$Time), format="%Y-%m-%d %H:%M:%S")
  data
}

data <- readData("data//household_power_consumption.txt")

with(data, plot(datetime, Global_active_power, type="l", xlab="", ylab="Globle Active Power (kilowatts)"))

## copy plot to png file
dev.copy(png, file="assignment-solution/plot2.png")
dev.off()

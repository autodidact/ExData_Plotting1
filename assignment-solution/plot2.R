readData <- function(filename) {  
  # First we will just read the first column from data and find our data boundaries
  # to use later when we read the full data.
  classes <- c("character", rep("NULL", times=8)) # read only first column
  data <- read.table(filename, header=TRUE, sep=";", na.strings="?", colClasses=classes)
  
  ## transform first column to a date column for comparison
  data$Date <- as.Date(data$Date, format="%d/%m/%Y") 
  
  min_row <- which.max(data$Date == as.Date("2007-02-01")) #first row to read
  max_row <- which.max(data$Date > as.Date("2007-02-02")) #first row to not read
  
  skip <- min_row ## number of rows to skip including header 
  nrows <- max_row - min_row 
  colNames <- c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", 
                "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
  
  classes <- c("character","character",rep("numeric", times=7))
  
  ## read all columns but only the rows that we require as computed above.
  data <- read.table(filename, header=FALSE, sep=";", na.strings="?", colClasses=classes, col.names=colNames, skip=skip, nrows=nrows)
  data$datetime = strptime(paste(data$Date, data$Time), format="%d/%m/%Y %H:%M:%S")
  
  data
}

data <- readData("data//household_power_consumption.txt")

# open png device
png(file="assignment-solution/plot2.png")

par(mfrow=c(1,1))
with(data, plot(datetime, Global_active_power, type="l", xlab="", ylab="Globle Active Power (kilowatts)"))

## copy plot to png file
#dev.copy(png, file="assignment-solution/plot2.png")
dev.off()

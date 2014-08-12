# 
# Creates the plot 4 Global Active Power for the first course project in the 
# Exploratory Data Analysis course
# 
createPlot4 = function () {
  library(data.table)
  # Load the dataset
  householdPowerConsumptionDataset <- fread("household_power_consumption.txt", sep = ";", header = T, 
                                            na.strings = "?")
  # Converts the Date column from character to Date type
  householdPowerConsumptionDataset$Date <- as.Date(as.character(householdPowerConsumptionDataset$Date), 
                                                   format = "%d/%m/%Y")
  
  # Subset the data to work with
  powerConsuptionToAnalize <- subset(householdPowerConsumptionDataset, Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"))
  # Removes the big dataset from memory
  rm(householdPowerConsumptionDataset)
  
  # Make the Global_active_power variable a numeric one
  powerConsuptionToAnalize$Global_active_power <- as.numeric(powerConsuptionToAnalize$Global_active_power)
  
  powerConsuptionToAnalize$WeekDay <- as.POSIXct(paste(powerConsuptionToAnalize$Date, powerConsuptionToAnalize$Time), 
                                                 format="%Y-%m-%d %H:%M:%S")
  
  # Define that we are going to put 4 images in one device
  par(mfrow=c(2,2))
  
  # Draw the Global Active Power plot
  globalActivePower()
  
  # Draw the datetime by Voltage plot
  datetimeByVoltage()
  
  # Draw the Energy sub metering plot
  energySubMetering()

  # Draw the global reactive power plot
  datetimeByBlobalReactivePower()
  
  dev.copy(device = png, file = "plot4.png",  bg = "white", width = 480, height = 480)
  dev.off()
} 

# 
# Defines the Global Active Power graph.
# 
globalActivePower = function() {
  # Plot lines
  plot(powerConsuptionToAnalize$WeekDay, powerConsuptionToAnalize$Global_active_power, type="l", main="", 
       ylab = "Global Active Power", xlab = "", lwd=1)
} 

# 
# Draw the datetime by Voltage graph.
# 
datetimeByVoltage = function() {
  # Plot lines
  plot(powerConsuptionToAnalize$WeekDay, powerConsuptionToAnalize$Voltage, type="l", main="", 
       ylab = "Voltage", xlab = "datetime", lwd=1)
} 

# 
# Draw the datetime by Voltage graph.
# 
energySubMetering = function() {
  # Plot lines
  plot_colors <- c("black","red","blue")
  par(mar = c(4, 5, 2, 2))
  plot(powerConsuptionToAnalize$WeekDay, powerConsuptionToAnalize$Sub_metering_1, type="l", lwd=1, col = plot_colors[1], 
       main = "", ylab = "Energy sub metering", xlab = "")
  lines(powerConsuptionToAnalize$WeekDay, powerConsuptionToAnalize$Sub_metering_2, type="l", lwd=1, col = plot_colors[2])
  lines(powerConsuptionToAnalize$WeekDay, powerConsuptionToAnalize$Sub_metering_3, type="l", lwd=1, col = plot_colors[3])
  
  legend("topright", names(powerConsuptionToAnalize)[7:9], col=plot_colors, lty = 1);
} 

# 
# Draw the datetime by Global reactive power
# 
datetimeByBlobalReactivePower = function() {
  # Plot lines
  plot(powerConsuptionToAnalize$WeekDay, powerConsuptionToAnalize$Global_reactive_power, type="l", main="", 
       ylab = "Global_reactive_power", xlab = "datetime", lwd=1)
}
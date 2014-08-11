# 
# Creates the plot 3 Global Active Power for the first course project in the 
# Exploratory Data Analysis course
# The plot is a lines graph on the sub metering 1, sub metering 2 and sub metering 3 variables.
# 
createPlot3 = function () {
  library(data.table)
  # Load the dataset
  householdPowerConsumptionDataset <- fread("household_power_consumption.txt", sep = ";", header = T, na.strings = "?")
  # Converts the Date column from character to Date type
  householdPowerConsumptionDataset$Date <- as.Date(as.character(householdPowerConsumptionDataset$Date), format = "%d/%m/%Y")
  
  # Subset the data to work with
  powerConsuptionToAnalize <- subset(householdPowerConsumptionDataset, Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"))
  # Removes the big dataset from memory
  rm(householdPowerConsumptionDataset)
  
  # Make the Global_active_power variable a numeric one
  powerConsuptionToAnalize$Global_active_power <- as.numeric(powerConsuptionToAnalize$Global_active_power)
  
  powerConsuptionToAnalize$WeekDay <- as.POSIXct(paste(powerConsuptionToAnalize$Date, powerConsuptionToAnalize$Time), format="%Y-%m-%d %H:%M:%S")
  
  # Plot lines
  plot_colors <- c("black","red","blue")
  par(mar = c(4, 5, 2, 2))
  plot(powerConsuptionToAnalize$WeekDay, powerConsuptionToAnalize$Sub_metering_1, type="l", lwd=1, col = plot_colors[1], 
        main = "", ylab = "Energy sub metering", xlab = "")
  lines(powerConsuptionToAnalize$WeekDay, powerConsuptionToAnalize$Sub_metering_2, type="l", lwd=1, col = plot_colors[2])
  lines(powerConsuptionToAnalize$WeekDay, powerConsuptionToAnalize$Sub_metering_3, type="l", lwd=1, col = plot_colors[3])
  
  legend("topright", names(powerConsuptionToAnalize)[7:9], col=plot_colors, lty = 1);
  
  dev.copy(device = png, file = "plot3.png",  bg = "white", width = 480, height = 480)
  dev.off()
}
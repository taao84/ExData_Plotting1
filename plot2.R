# 
# Creates the plot 1 Global Active Power for the first course project in the 
# Exploratory Data Analysis course
# The plot is a histogram on the Global_active_power variable.
# 
createPlot2 = function () {
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
  par(mar = c(4, 5, 2, 2))
  plot(powerConsuptionToAnalize$WeekDay, powerConsuptionToAnalize$Global_active_power, type="n")
  plot(powerConsuptionToAnalize$WeekDay, powerConsuptionToAnalize$Global_active_power, type="l", main="", 
       ylab = "Global Active Power(kilowatts)", xlab = "", lwd=1)
  
  dev.copy(device = png, file = "plot2.png",  bg = "white")
  dev.off()
}
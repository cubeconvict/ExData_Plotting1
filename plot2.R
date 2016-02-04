
# Loading the data 
#When loading the dataset into R, please consider the following:

#The dataset has 2,075,259 rows and 9 columns. First calculate a rough estimate of how much memory the dataset will require in memory before reading into R. Make sure your computer has enough memory (most modern computers should be fine).
# We will only be using data from the dates 2007-02-01 and 2007-02-02. 
# One alternative is to read the data from just those dates rather than reading in the entire dataset and subsetting to those dates.
# You may find it useful to convert the Date and Time variables to Date/Time classes in R using the strptime()  and as.Date() functions.
# Note that in this dataset missing values are coded as ?.

# Making Plots 
# Our overall goal here is simply to examine how household energy usage varies over a 2-day period in February, 2007. 
# Your task is to reconstruct the following plots below, all of which were constructed using the base plotting system.

# First you will need to fork and clone the following GitHub repository: https://github.com/rdpeng/ExData_Plotting1

# For each plot you should

# Construct the plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels.
# Name each of the plot files as plot1.png, plot2.png, etc.
# Create a separate R code file (plot1.R, plot2.R, etc.) that constructs the corresponding plot, i.e. code in plot1.R constructs the plot1.png plot. 
# Your code file should include code for reading the data so that the plot can be fully reproduced. You must also include the code that creates the PNG file.
# Add the PNG file and R code file to the top-level folder of your git repository (no need for separate sub-folders)
# When you are finished with the assignment, push your git repository to GitHub so that the GitHub version of your repository is up to date. 
# There should be four PNG files and four R code files, a total of eight files in the top-level folder of the repo.

fulldatafile = "./data/household_power_consumption.txt"


testdimensions <- function(mydata.table){

	library(pryr)     
	#tell the program where file is found
	print("Full data file being read")
	print(dim(mydata.table))
	print("Object size")
	print(object_size(mydata.table))

}



createmyplot2 <- function(mydata, outputfilename) {
  print("Creating plot2")
  #x=Global Active Power(kilowatts)
  #y=Days (Thu, Fri, Sat)
  # Type of graph: line
  
  
  
  png(file = outputfilename, width = 480, height = 480) ## Open the PNG Device
  
  plot(mydata$datetime, mydata$Global_active_power, type="n", ylab = "Global Active Power (kilowatts)", xlab='')
  lines(mydata$datetime, mydata$Global_active_power, type="l")
  
  dev.off()
  cat(outputfilename, " written to disc")
}




formatmydata <- function(unformatteddata.table) {
  
  
  unformatteddata.table$Date <- as.Date(unformatteddata.table$Date, "%d/%m/%Y")
  unformatteddata.table$Global_active_power <- as.numeric(unformatteddata.table$Global_active_power)
  #unformatteddata.table$Time <- strptime(unformatteddata.table$Time, "")
  unformatteddata.table["datetime"] <- as.POSIXct(paste(unformatteddata.table$Date, unformatteddata.table$Time), format="%Y-%m-%d %H:%M:%S")
  formatteddata.table <- unformatteddata.table
  return(formatteddata.table)
}

subsetdata <- function(fulldatafile, startdate, enddate) {
  
  startdate = "2007-02-01"
  enddate = "2007-02-02"
  
  #read the data in and view the header
  alldata <- read.table(fulldatafile, sep=";", header = TRUE)
  print("Reading data")
  #print(head(alldata))
  
  #format the data
  print("Formatting data")
  formatteddata <- formatmydata(alldata)
  #print(head(formatteddata))
  
  
  print("Subsetting the data.")
  dateconstraineddata <- formatteddata[formatteddata$Date == "2007-02-01" | formatteddata$Date == "2007-02-02",]
  #print(head(dateconstraineddata))
  
  return (dateconstraineddata)
}


	mydata <- subsetdata(fulldatafile)
	createmyplot2(mydata, "plot2.png")


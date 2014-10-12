## This script is used load the data to R 
## subsetting the data from the dates 2007-02-01 and 2007-02-02
## create the plot4 image file


## This function is used to load and subset the data
load <- function(){
  #load library
  require(lubridate)
  require(tidyr)
  require(dplyr)
  
  #FILE NAME
  FILE <- "./data/household_power_consumption.txt"
  
  
  if(!file.exists(FILE)){
    error_msg <- paste("Data file does not exists:",FILE,sep=" ")
    stop(error_msg)
  }
  
  #Load the data and 
  message("Load the data....")
  origData <- read.csv(FILE,sep=";",stringsAsFactors=FALSE
                       ,colClass=c("character","character","numeric"
                                   ,"numeric","numeric","numeric"
                                   ,"numeric","numeric","numeric")
                       ,na.strings="?")
  
  
  message("Subset the data....")
  subsetData <-
    origData %>%
    unite("Date_time_str",Date,Time, sep=" ") %>%
    mutate(Date_time = dmy_hms(Date_time_str)) %>%
    select(-Date_time_str) %>%
    filter(Date_time >= ymd("2007-02-01")) %>%
    filter(Date_time <ymd("2007-02-03"))
  
}

##Plotting plot3
plotOutput <- function(){
  
  # Load data
  if(!file.exists("plot")){
    dir.create("plot")
  } 
  
  data <- load()
  
  #Ploting
  message("Export plotting graphics.....")
  Sys.setlocale("LC_TIME", "US")
  png(file = "./plot/plot4.png")
  
  par(mfrow=c(2,2))
  
  # plot 1,1
  plot(data$Date_time,data$Global_active_power,type="l"
       ,ylab="Global Active Power"
       ,xlab="")
  
  # plot 1,2
  plot(data$Date_time,data$Voltage,type="l"
       ,ylab="Voltage"
       ,xlab="datetime")
  
  # plot 2,1
  
  plot(data$Date_time,data$Sub_metering_1,type="l"
       ,ylab="Energe sub metering"
       ,xlab="")
  points(data$Date_time,data$Sub_metering_2,type="l",col="red")
  points(data$Date_time,data$Sub_metering_3,type="l",col="blue")
  legend("topright", lty=1,col = c("black","blue", "red")
         , legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
         , bty="n")
  
  # plot 2,2
  plot(data$Date_time,data$Global_reactive_power,type="l"
       ,ylab="Global_reactive_power"
       ,xlab="datetime")
  
  dev.off() 
}

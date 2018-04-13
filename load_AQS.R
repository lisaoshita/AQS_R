# ======================================= 
# Function to load AQS data
# ======================================= 


# packages used in read.aqs
library(dplyr)
library(magrittr)
library(rebus)


# function will take file name as character string
# file must be in working directory

read.aqs <- function(filename, level) {
  
  data <- read.table(filename, sep = "|", header = TRUE) # read in file
  
  if (level == 0) { # level 0: no simplification 
    data <- data %>% mutate_at(as.character, # set column classes
                               .vars = c("State.Code", "County.Code", "Site.ID", 
                                         "Parameter", "POC", "Sample.Duration",
                                         "Unit", "Method", "Date", "Sampling.Frequency",
                                         "Monitor.Protocol..MP..ID", 
                                         "Alternate.Method.Detectable.Limit"))
  }
  if (level == 1) {
    # convert dates and start.times to POSIX
    data$Date.Start.time <- as.POSIXct(paste(paste(substr(data$Date, 1, 4), 
                                  substr(data$Date, 5, 6), 
                                  substr(data$Date, 7, 8), 
                                  sep = "-"), data$Start.Time, sep = " "), 
                            format = "%Y-%d-%m %H:%M", tz = "UTC")
    # drop unecessary columns
    data <- data %>% select(-c(which(colnames(data) %in% c("Date", "Start.time", "RD",
                                                           "Action.Code")), 14:28))
  }
  return(data)
}




data <- read.aqs(filename = "AMP501_1595753-0.txt", level = 1)










data <- read.table("AMP501_1595753-0.txt", sep = "|", header = TRUE)






















# =========================================================================== 
# Function to load AQS data
# =========================================================================== 


original <- read.table("AMP501_1595753-0.txt", sep = "|", header = TRUE,
                       colClasses = c(rep("character", 12), "numeric", 
                                      rep("character", 13)))

# packages used in read.aqs
library(dplyr)
library(magrittr)
library(rebus)
library(stringr)

# load csv files with monitor lables


# function will take file name as character string
# file must be in working directory

read.aqs <- function(filename, level = 2) {
  # -----------------------------------------------------------
  data <- read.table(file = filename, # read in data
                     sep = "|", 
                     header = TRUE,
                     colClasses = c(rep("character", 12), 
                                    "numeric", 
                                    rep("character", 13)))
  if (level == 0) {
    return(data)
  }
  # -----------------------------------------------------------
  # convert dates and start.times to POSIX
  data$Date.Start.time <- as.POSIXct(paste(paste(substr(data$Date, 1, 4), 
                                  substr(data$Date, 5, 6), 
                                  substr(data$Date, 7, 8), 
                                  sep = "-"), data$Start.Time, sep = " "), 
                            format = "%Y-%d-%m %H:%M", tz = "UTC")
  # drop unecessary columns
  data <- data %>% select(-c(which(colnames(data) %in% c("Date", "Start.Time", "RD",
                                                           "Action.Code")), 14:28))
  if (level == 1) {
    return(data)
  }
  # -----------------------------------------------------------
  # default level, concatenate monitor IDs
  data$monitor.ID <- paste(data$State.Code,
                           data$County.Code,
                           data$Site.ID,
                           data$Parameter,
                           data$POC,
                           data$Sample.Duration,
                           data$Unit,
                           data$Method,
                           sep = "-")
  
  if (level == 2) { # stop if level = 2, drop all unecessary columns, reorder columns
    
    data <- data %>% select(-c(1:8))
    data <- data[c("monitor.ID", "Date.Start.time", "Sample.Value")]
    
    return(data)
  }
  # -----------------------------------------------------------
  # apply monitor names 
  data$code <- paste(data$State.Code, data$County.Code, sep = "-")
  data <- data %>% left_join(regions, by = "code")
  data <- data %>% left_join(parameter, by = "Parameter")
  data <- data %>% left_join(durations, by = "Sample.Duration")
  data <- data %>% left_join(units, by = "Unit")
  data <- data %>% left_join(methods, by = "Method")
  
  data$monitor.label <- paste(data$region,
                              data$Site.ID,
                              data$parameter_label,
                              data$POC,
                              data$Duration.Description,
                              data$unit_label,
                              data$method_label,
                              sep = "-")
  data <- data %>% select(monitor.label, Date.Start.time, Sample.Value, )

  if (level == 3) {
    return(data)
  }
  # -----------------------------------------------------------
  
}

data <- read.aqs(filename = "AMP501_1595753-0.txt", level = 3)


# still working on this: 






# concatenating all labels


















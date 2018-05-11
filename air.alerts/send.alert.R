# ===========================
# AirAware Text Alert Script
# ===========================

# Only send alert when:

# CDF PM10 > 175 and
# CDF PM10 < 800 and
# The hour that’s >175 is after 8:00 PST, and
# An alert has not already been sent earlier in the day.


# =============================
# Function to send text alerts
# =============================


# function takes filename as a character string
# options(show.error.messages = FALSE)
# options(warn = -1) 

text.alert <- function(filename) {
  
  file <- read.csv(filename, 
                   header = F,
                   skip = 3,
                   col.names = c("Date", "pm10"),
                   colClasses = c("character", "numeric"))
  
  file$Date <- as.POSIXct(file$Date, 
                          format = "%d-%b-%Y %H:%M", 
                          tz = "UTC")
  
  if (as.Date(file$Date[1]) != Sys.Date()) return("Old file") 
  
  file$pm10[file$pm10 >= 800] <- NA
  
  # set pm10 to NA if pm10 > 175 and time < 8:00
  time.threshold <- as.POSIXct(paste(substr(file$Date[1], 1, 10), "08:00:00"), tz = "UTC") 
  file$pm10[which((file$Date <= time.threshold) & (file$pm10 > 175))] <- NA
  
  # check pm10 values again 
  if (sum(file$pm10 < 175, na.rm = T) == sum(!is.na(file$pm10))) return("No text sent") 

  # check logs 
  log <- read.csv(file = "//apcd04/data/TECH/AirVision/Reportfiles/AV_R_scripts/alert.log.csv", 
                  header = TRUE,
                  colClasses = rep("character", 2))
  
  dates <- as.Date(substr(log$Date.Sent, 1, 10), format = "%Y-%m-%d")

  # if date in log == present date, stop 
  if (sum(dates == Sys.Date()) > 0) return("Text already sent today") 
  
  # intialize empty df for text info
  alert.log <- data.frame(Date.Sent = NA, 
                          Body = "Texts sent!")
  
  # run python script
  run <- system2(command = "python", args = "//apcd04/data/TECH/AirVision/Reportfiles/AV_R_scripts/alert.py") 
  
  # if system2 command returns errors, stop function here
  if (run != 0) return("Texts failed to send")

  # update alert.log
  alert.log$Date.Sent <- Sys.time()

  # append new message information to alert.log.csv
  cat("\n", file = '//apcd04/data/TECH/AirVision/Reportfiles/AV_R_scripts/alert.log.csv', append = TRUE)
  write.table(alert.log,
              sep = ",",
              file = "//apcd04/data/TECH/AirVision/Reportfiles/AV_R_scripts/alert.log.csv",
              col.names = F,
              row.names = F,
              append = T)
}

text.alert(filename = "//apcd04/data/TECH/AirVision/Reportfiles/CDFPM10.csv")
rm(list=ls())




# ===========================
# AirAware Text Alert Script
# ===========================

# Only send alert when:

# CDF PM10 > 175 and
# CDF PM10 < 800 and
# The hour that’s >175 is after 8:00 PST, and
# An alert has not already been sent earlier in the day.

# -----------------------------------------

library(twilio)

# load data (for testing)
file <- read.csv(file = "air.alerts/pm10_test.csv", header = TRUE, stringsAsFactors = FALSE)
# convert Date to Posix
file$Date <- as.POSIXct(file$Date, format = "%m/%d/%y %H:%M", tz = "UTC")
file$Date[1] == Sys.Date()

# =============================
# Function to send text alerts
# =============================


# function takes filename as a character string
# note: cdf value in file renamed to pm10 


text.alert <- function(filename) {
  
  file <- read.csv(filename, 
                   header = F,
                   skip = 3,
                   col.names = c("Date", "pm10"),
                   colClasses = c("character", "numeric"))

  file$Date <- as.POSIXct(file$Date, 
                          format = "%d-%B-%Y %H:%M", 
                          tz = "UTC")
  
  if (as.Date(file$Date[1]) != Sys.Date()) return("Old file")
  
  file$pm10[file$pm10 >= 800] <- NA
  
  # stop and return message if all pm10 < 175
  if (sum(file$pm10 < 175, na.rm = T) == sum(!is.na(file$pm10))) return("No text sent")
  
  # set pm10 to NA if pm10 > 175 and time < 8:00
  time.threshold <- as.POSIXct(paste(substr(file$Date[1], 1, 10), "08:00:00"), tz = "UTC") 
  file$pm10[which((file$Date <= time.threshold) & (file$pm10 > 175))] <- NA
  
  # check pm10 values again 
  if (sum(file$pm10 < 175, na.rm = T) == sum(!is.na(file$pm10))) return("No text sent")

  # check logs 
  log <- read.csv(file = "air.alerts/alert.log.csv", 
                  header = TRUE,
                  colClasses = rep("character", 2))
  
  dates <- as.Date(substr(log$Date.Sent, 1, 10), format = "%Y-%m-%d")

  # if date in log == present date, stop 
  if (sum(dates == Sys.Date()) > 0) return("Text already sent today") 
  
  # intialize empty df for text info
  alert.log <- data.frame(Date.Sent = NA, 
                          Body = NA)
  
  # run python script from batch file
  writeLines(c("H:",
               "cd /TECH/Lisa/R/apcd.r/air.alerts",
               "python alert.py"),
             con = "air.alerts/774R3UvON5.bat")
  
  run <- system2("air.alerts/774R3UvON5.bat")
  
  # if system2 command returns errors, stop function here
  if (run != 0) return("Texts failed to send")

  # update alert.log
  alert.log$Date.Sent <- Sys.time()

  Sys.setenv(TWILIO_SID = "xx")
  Sys.setenv(TWILIO_TOKEN = "xx")

  message.log <- twilio::tw_get_messages_list()[[1]] # list of messages (1 = most recent text)

  alert.log$Body <- message.log$body

  # append new message information to alert.log.csv
  cat("\n", file = 'air.alerts/alert.log.csv', append = TRUE)
  write.table(alert.log,
              sep = ",",
              file = "air.alerts/alert.log.csv",
              col.names = F,
              row.names = F,
              append = T)
  
  unlink("air.alerts/774R3UvON5.bat") # delete batch file

}

text.alert(filename = "C:/Users/loshita/Desktop/CDFPM10.csv")


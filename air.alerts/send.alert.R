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
file <- read.csv(file = "air.alerts/CDFPM10.csv", header = TRUE, stringsAsFactors = FALSE)
# convert Date to Posix
file$Date <- as.POSIXct(file$Date, format = "%m/%d/%Y %H:%M", tz = "UTC")

# change from 44 to 200 (for testing)
file$pm10[12] <- 200
file$pm10[12] <- 44


# =============================
# Function to send text alerts
# =============================

# function takes filename as a character string
# function: 
#   - reads in pm10 file and formats Date
#   - first if statement:
#         - checks if any pm10 values > 175 & < 800
#         - if true: run second if statement
#         - if false: print either "PM10 level above 800 detected" or "PM10 levels below 175"
#   - second if statement: 
#         - checks if time of pm10 value that is greater than 175, is < 8
#         - if true: prints "Time < 8:00" 
#         - if false: runs third if statement
#   - third if statement:
#         - uploads the log of messages sent
#         - checks if any of the dates in the log are equal to the present date (change this?)
#         - if false: send SMS message
#         - if true: print "Text already sent today. No text alert sent." 
# NOTE: SID, Token, and to and from numbers deleted from file
# all times saved in UTC timezone format


text.alert <- function(filename) {
  
  # load the data -------------------
  file <- read.csv(file = filename, header = TRUE, stringsAsFactors = FALSE)
  file$Date <- as.POSIXct(file$Date, format = "%m/%d/%Y %H:%M", tz = "UTC") 
  
  # 1st IF statement ----------------
  if (sum(file$pm10 >= 175, na.rm = T) > 0 & 
      sum(file$pm10 >= 800, na.rm = T) == 0) {
    
    time <- as.numeric(format(file$Date[which(file$pm10 >= 175)], "%H"))
    
    # 2nd IF statement -------------
    if (time < 8) {
      
      print("Time < 8:00")
    
    } else if (time > 8) {
      
      log <- read.csv(file = "air.alerts/alert.log.csv",
                      header = TRUE)
      
      dates <- as.Date(substr(log$Date.Sent, 1, 10), format = "%Y-%m-%d")
      
      # 3rd IF statement -----------
      if (sum(dates == Sys.Date()) > 0) {
        
        print("Text already sent today. No text alert sent.")
        
      } else {
        
        alert.log <- data.frame(Date.Sent = NA, # intialize empty df for text info 
                                To = NA,
                                From = NA,
                                Body = NA,
                                Status = NA, 
                                Error.Code = NA,
                                Error.Message = NA)
        
        # send SMS message ---------------------------------------
        Sys.setenv(TWILIO_SID = "ACa471eea1d61917cce7d77ac2c1637889")
        Sys.setenv(TWILIO_TOKEN = "a31cfe18cbdff3a5297d4dd9df941042")
        
        tw_send_message(to = "9169499719", 
                        from = "9168238560", 
                        body = "(EARLY AIRAWARE ALERT) Blowing dust detected on the Nipomo Mesa. Visit AIRNOW <http://bit.ly/NipomoAQI>, to monitor the hourly AQI.")
        
        # update alert.log ---------------------------------------
        alert.log$Date.Sent <- Sys.time()
        
        message.log <- tw_get_messages_list()[[1]] # list of messages (1 = most recent text)
        
        alert.log$To <- message.log$to
        alert.log$From <- message.log$from
        alert.log$Body <- message.log$body
        alert.log$Status <- message.log$status
        
        if (is.null(message.log$error_code)) {
          alert.log$Error.Code <- NA
          alert.log$Error.Message <- NA
          
        } else {
          
          alert.log$Error.Code <- message.log$error_code
          alert.log$Error.Message <- message.log$error_message
        
        }
        
        # append new message information to alert.log.csv
        cat("\n", file = 'air.alerts/alert.log.csv', append = TRUE)
        write.table(alert.log,
                    sep = ",",
                    file = "air.alerts/alert.log.csv",
                    col.names = F,
                    row.names = F,
                    append = T)
        
        print("Text alert sent. Log updated.")
        
      }
    }
  } else if (sum(file$pm10 >= 175, na.rm = T) > 0 & 
             sum(file$pm10 >= 800, na.rm = T) > 0) {
    
    print("PM10 level above 800 detected")
    
  } else {
    
    print("PM10 levels below 175")
    
  }
  
}

text.alert(filename = "air.alerts/CDFPM10.csv")



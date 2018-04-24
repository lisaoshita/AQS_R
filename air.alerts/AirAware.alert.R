# ================
# AirVision Script
# ================

# Only send alert when:

# CDF PM10 > 175 and
# CDF PM10 < 800 and
# The hour that’s >175 is after 8:00 PST, and
# An alert has not already been sent earlier in the day.

# -----------------------------------------

library(twilio)

# load data
file <- read.csv(file = "air.alerts/CDFPM10.csv", header = TRUE, stringsAsFactors = FALSE)

# convert Date to Posix
file$Date <- as.POSIXct(file$Date, format = "%m/%d/%Y %H:%M", tz = "UTC") # timezone? 


# change from 44 to 200 (for testing)
file$pm10[12] <- 200

alert.log <- data.frame(Date.Created = NA,
                        Body = NA,
                        Status = NA, 
                        Error.Code = NA,
                        Error.Message = NA)

# ==============================================
# SCRIPT
# ==============================================

if (sum(file$pm10 >= 175, na.rm = T) > 0 & 
    sum(file$pm10 >= 800, na.rm = T) == 0) {
  
  # -----------
  # check time
  # -----------
  
  time <- as.numeric(format(file$Date[which(file$pm10 >= 175)],"%H"))
  
  if (time > 8) {
    
    # ---------
    # check log
    # ---------
    
    # log <- read.csv("air.alerts/alert.log.csv",
    #                 header = T,
    #                 stringsAsFactors = F)
    
    # fix this 
    
    # --------
    # send SMS
    # --------
    
    Sys.setenv(TWILIO_SID = "sid")
    Sys.setenv(TWILIO_TOKEN = "token")
    
    tw_send_message(to = "9169499719", 
                    from = "num", 
                    body = "TEXT ALERT blahbalhbalh")
    
    # ----------
    # update log
    # ----------
    messages.log <- tw_get_messages_list()
    
    alert.log$Date.Created <- messages.log[[1]]$date_created # reformat this date - turn into date + time 
    alert.log$Body <- messages.log[[1]]$body
    alert.log$Status <- messages.log[[1]]$status
    
    if (is.null(messages.log[[1]]$error_code)) {
      alert.log$Error.Code <- "None"
      alert.log$Error.Message <- "None"
    }

    write.table(alert.log, 
                file = "air.alerts/alert.log.csv",
                sep = ",",
                col.names = F,
                row.names = F, 
                append = T)

  }
} 











# ================
# AirVision Script
# ================

# Only send alert when:

# CDF PM10 > 175 and
# CDF PM10 < 800 and
# The hour that’s >175 is after 8:00 PST, and
# An alert has not already been sent earlier in the day.

# -----------------------------------------

# load data
file <- read.csv(file = "CDFPM10.csv", header = TRUE, stringsAsFactors = FALSE)

# convert Date to Posix
file$Date <- as.POSIXct(file$Date, format = "%m/%d/%Y %H:%M", tz = "UTC") # timezone? 
pm10 <- file$pm10


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
    sum(file$pm10 >= 800, na.rm = T) == 0 & 
    file$Date[which(file$pm10 > 175)] > as.POSIXct("8:00", format = "%H:%M", tz = "UTC")) {
  
  # ---------
  # check log
  # ---------
  
  # --------
  # send SMS
  # --------
  Sys.setenv(TWILIO_SID = sid)
  Sys.setenv(TWILIO_TOKEN = token)
  
  tw_send_message(to = "9169499719", 
                  from = "number", 
                  body = "TEXT ALERT���")
  
  # ----------
  # update log
  # ----------
  messages.log <- tw_get_messages_list()
  
  alert.log$Date.Created <- messages.log[[length(messages.log)]]$date_created
  alert.log$Body <- messages.log[[length(messages.log)]]$body
  alert.log$Status <- messages.log[[length(messages.log)]]$status
  
  if (is.null(messages.log[[length(messages.log)]]$error_code)) {
    alert.log$Error.Code <- "None"
    alert.log$Error.Message <- "None"
  }
  
  write.table(alert.log, 
              file = "alert.log.txt", 
              append = T, 
              row.names = F,
              col.names = T,
              sep = "|")
  
} 











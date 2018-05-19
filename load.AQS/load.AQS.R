# =========================================================================== 
# Function to load AQS data
# =========================================================================== 


original <- read.table("load.AQS/AMP501_1595753-0.txt", sep = "|", header = TRUE,
                       colClasses = c(rep("character", 12), "numeric", 
                                      rep("character", 13)))


library(reshape2) # used in read.aqs for level = 4


# function will take file name as character string

# ==================
# read.aqs function
# ==================

read.aqs <- function(filename, level = 2, time.zone = "UTC", remove = FALSE) {
  
  # ========
  # LEVEL 0
  # ========
  data <- read.table(file = filename, # read in data
                     sep = "|", 
                     header = TRUE,
                     colClasses = c(rep("character", 12), 
                                    "numeric", 
                                    rep("character", 13)))
  
  # remove columns with all same value if remove = TRUE
  if (remove == TRUE) {
    rcol <- rep(0, ncol(data))
    for (i in 1 : ncol(data)) {
      
      if (length(unique(data[ , i])) == 1) {
        rcol[i] <- 1
      }
    }
    # removes all columns with same values (except date column)
    data <- data[ , -which((rcol == 1) & (colnames(data) != "Date"))]
  }
  
  if (level == 0) return(data)
  
  # ========
  # LEVEL 1
  # ========
  data$Date.Time <- as.POSIXct(paste(paste(substr(data$Date, 1, 4), # convert date + start time to POSIX
                                           substr(data$Date, 5, 6), 
                                           substr(data$Date, 7, 8), 
                                           sep = "-"), data$Start.Time, sep = " "), 
                               format = "%Y-%m-%d %H:%M", tz = time.zone)
  
  # drop unecessary columns
  data <- data[ , -c((which(colnames(data) %in% c("Date", 
                                                  "Start.Time",
                                                  "RD", 
                                                  "Action.Code", 
                                                  "Null.Data.Code",
                                                  "Sampling.Frequency",
                                                  "Monitor.Protocol..MP..ID", 
                                                  "Alternate.Method.Detectable.Limit", 
                                                  "Uncertainty"))),
                     (which(startsWith(colnames(data), "Qualifier") == TRUE)))]
  
  
  if (level == 1) return(data)
  
  # ========
  # LEVEL 2 (default level)
  # ========
  
  # concatenating monitor labels
  paste.args <- c(data[, -c(which(colnames(data) == "Sample.Value"), 
                            which(colnames(data) == "Date.Time"))], 
                  sep="/")
  
  data$Monitor.ID <- do.call(paste, paste.args)
  
  if (level == 2) {
    
    # data <- data[c("Monitor.ID", "Date.Time", "Sample.Value")] # reorders columns
    
    return(data)
  }
  
  # ========
  # LEVEL 3
  # ========
  
  to.upload <- colnames(test2)[!(colnames(test2) %in% c("Site.ID", 
                                                        "POC", 
                                                        "Sample.Value", 
                                                        "Date.Time",
                                                        "Monitor.ID"))]

  # load only the data sets needed, join with data by shared column 
  for (i in to.upload) {

    label <- get.labels(i)
    data <- merge(data,
                  label,
                  by = i,
                  all.x = TRUE)
  }

  
  # 
  # # Concatenating all columns 
  # data$Monitor.Label <- paste(data$region, (STATE THEN COUNTY)
  #                             data$Site.ID,
  #                             data$parameter.label,
  #                             data$POC,
  #                             data$Duration.Description,
  #                             data$Unit.Label,
  #                             data$Method.Label,
  #                             sep = "/")
  # 
  # data <- data[, c("Monitor.Label", "Date.Time", "Sample.Value")]

  if (level == 3) return(data)
    
  # ========
  # LEVEL 4
  # ========
  
  # Concatenating all columns 
  data$Monitor.Label <- paste(data$region,
                              data$Site.ID,
                              data$parameter.label,
                              data$POC,
                              data$Duration.Description,
                              data$Unit.Label,
                              data$Method.Label,
                              sep = "/")
  
  # Long -> wide format 
  data <- reshape2::dcast(data[, c("Monitor.Label", "Date.Time", "Sample.Value")], 
                          Date.Time ~ Monitor.Label, 
                          value.var = "Sample.Value",
                          na.rm = TRUE,
                          fill = 0)
    
  if (level == 4) return(data)



}
  

# used inside the function
get.labels <- function(filename) {
  
  data <- read.table(file = paste("load.AQS/monitor.labels/", 
                                  filename, 
                                  ".txt", 
                                  sep = ""),
                     sep = "|",
                     header = TRUE,
                     colClasses = c("character", "character"))
  
  return(data)
  
}


test2 <- read.aqs(filename = "load.AQS/AMP501_1595753-0.txt", level = 3, remove = TRUE)


# STILL WORKING ON: 

# REFORMAT: MONITOR LABELS
# - CHANGE WHITE SPACES IN LABELS TO "-"
# - CONVERT METHOD LABELS TO ALL LOWERCASE
# CLEAN UP LABEL.SETUP SCRIPT
# FIND A WAY TO CONCATENATE COLUMNS IN THE RIGHT ORDER!!!!!



  





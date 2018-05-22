# =========================================================================== 
# Function to load AQS data
# =========================================================================== 


original <- read.table("load.AQS/AMP501_1595753-0.txt", sep = "|", header = TRUE,
                       colClasses = c(rep("character", 12), "numeric", 
                                      rep("character", 13)))


library(reshape2) # used in read.aqs for level = 4


# function will take file name as character string

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
  
  state.code <- data$State.Code # for future use
  
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
                  sep="|")
  
  data$Monitor.ID <- do.call(paste, paste.args)
  
  if (level == 2) {
    
    data <- data[c("Date.Time", "Monitor.ID", "Sample.Value")] # reorders columns
    
    return(data)
  }
  
  # ========
  # LEVEL 3
  # ========
  
  to.upload <- colnames(data)[!(colnames(data) %in% c("Site.ID", 
                                                      "POC", 
                                                      "Sample.Value", 
                                                      "Date.Time",
                                                      "Monitor.ID"))]

  # load only the data sets needed, join with data by shared column 
  for (i in to.upload) {
    
    if (i == "County.Code") { 
      
      label.file <- get.labels("County")
      data$County.Code <- paste(state.code, data$County.Code, sep = "/")
      matches <- match(data$County.Code, label.file$Code)
      data$County.Code <- label.file$Region[matches]
      
    } else {
        
      label.file <- get.labels(i)
      var.name <- paste(i, ".Name", sep = "")
      
      data[[i]] <- label.file[match(data[[i]], label.file[[i]]), 2] 
      
      }
  }
  
  # Concatenating 
  paste.args1 <- c(data[, -c(which(colnames(data) == "Sample.Value"), 
                            which(colnames(data) == "Date.Time"),
                            which(colnames(data) == "Monitor.ID"))], 
                  sep="|")
  
  data$Monitor.ID <- do.call(paste, paste.args1)

  
  data <- data[, c("Date.Time", "Monitor.ID", "Sample.Value")]

  if (level == 3) return(data)
    
  # ========
  # LEVEL 4
  # ========
  
  # Long -> wide format 
  data <- reshape2::dcast(data, 
                          Date.Time ~ Monitor.ID, 
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


test2 <- read.aqs(filename = "load.AQS/AMP501_1595753-0.txt", level = 2, remove = T)


# STILL WORKING ON: 


# CLEAN UP LABEL.SETUP SCRIPT


# clean up state.code file (cut it down to unique values)
# parameter file has a lot of missing values 
# set function to report back if it can't find a matching label? and leave code as is









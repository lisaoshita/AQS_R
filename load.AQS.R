# =========================================================================== 
# Function to load AQS data
# =========================================================================== 


original <- read.table("AMP501_1595753-0.txt", sep = "|", header = TRUE,
                       colClasses = c(rep("character", 12), "numeric", 
                                      rep("character", 13)))


library(reshape2) # used in read.aqs for level = 4


# function will take file name as character string
# file must be in working directory


read.aqs <- function(filename, level = 2, time.zone = "UTC") {
  
  # -----------------------------------------------------------
  # ========
  # LEVEL 0
  # ========
  data <- read.table(file = filename, # read in data
                     sep = "|", 
                     header = TRUE,
                     colClasses = c(rep("character", 12), 
                                    "numeric", 
                                    rep("character", 13)))
  
  if (level == 0) return(data)
  
  # -----------------------------------------------------------
  # ========
  # LEVEL 1
  # ========
  data$Date.Time <- as.POSIXct(paste(paste(substr(data$Date, 1, 4), # convert date + start time to POSIX
                                           substr(data$Date, 5, 6), 
                                           substr(data$Date, 7, 8), 
                                           sep = "-"), data$Start.Time, sep = " "), 
                               format = "%Y-%m-%d %H:%M", tz = time.zone)
  # drop unecessary columns
  data <- data[ , -c(which(colnames(data) %in% c("Date", "Start.Time", "RD", "Action.Code")), 14:28)]
  
  if (level == 1) return(data)
  
  # -----------------------------------------------------------
  # ========
  # LEVEL 2 (default level)
  # ========
  data$Monitor.ID <- paste(data$State.Code, # concatenate monitor IDs
                           data$County.Code,
                           data$Site.ID,
                           data$Parameter,
                           data$POC,
                           data$Sample.Duration,
                           data$Unit,
                           data$Method,
                           sep = "-")
  
  if (level == 2) {
    
    data <- data[, -c(1:8)]
    data <- data[c("Monitor.ID", "Date.Time", "Sample.Value")]
    
    return(data)
  }
  
  # -----------------------------------------------------------
  # ========
  # LEVEL 3
  # ========
  
  # WRITE A FUNCT TO CONDENSE THIS SECTION
  
  # load data sets, join with data by shared column 
  regions <- read.table(file = "monitor.labels/regions.txt",
                        sep = "|",
                        header = TRUE,
                        colClasses = c("character", "character"))
  
  parameters <- read.table(file = "monitor.labels/parameters.txt", 
                           sep = "|",
                           header = TRUE,
                           colClasses = c("character", "character"))
  
  durations <- read.table(file = "monitor.labels/durations.txt", 
                          sep = "|",
                          header = TRUE,
                          colClasses = c("character", "character"))
  
  units <- read.table(file = "monitor.labels/units.txt",
                      sep = "|",
                      header = TRUE,
                      colClasses = c("character", "character"))
  
  methods <- read.table(file = "monitor.labels/methods.txt",
                        sep = "|",
                        header = TRUE,
                        colClasses = c("character", "character"))
  
  # Adding State - County labels 
  data$code <- paste(data$State.Code, data$County.Code, sep = "-")
  data <- merge(data, 
                regions, 
                by = "code", 
                all.x = TRUE)
  
  # Adding parameter labels
  data <- merge(data, 
                parameters, 
                by = "Parameter", 
                all.x = TRUE)
  
  # Adding sample duration labels
  data <- merge(data, 
                durations, 
                by = "Sample.Duration",
                all.x = TRUE)
  
  # Adding unit labels
  data <- merge(data, 
                units,
                by = "Unit", 
                all.x = TRUE)
  
  # Adding methods labels
  data <- merge(data,
                methods,
                by = "Method",
                all.x = TRUE)

  # Concatenating all columns 
  data$Monitor.Label <- paste(data$region,
                              data$Site.ID,
                              data$parameter.label,
                              data$POC,
                              data$Duration.Description,
                              data$Unit.Label,
                              data$Method.Label,
                              sep = "-")
  
  data <- data[, c("Monitor.Label", "Date.Time", "Sample.Value")]

  if (level == 3) return(data)
  
  # -----------------------------------------------------------
  # ========
  # LEVEL 4
  # ========
  
  data <- reshape2::dcast(data, # Long -> wide format 
                          Date.Time ~ Monitor.Label, 
                          value.var = "Sample.Value",
                          na.rm = TRUE,
                          fill = 0) 
  
  if (level == 4) return(data)

  
  
}


test <- read.aqs(filename = "AMP501_1595753-0.txt", level = 4)








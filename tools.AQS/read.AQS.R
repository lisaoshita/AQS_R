# =========================================================================== 
# Function to load AQS data
# =========================================================================== 



# function will take file name as character string

# header in AQS file must be uncommented before sending through the function
# changed parameter codes to the ones in parameter.csv on AQS site
# there are some missing values in Method.txt

# for this function include reshape library as suggests: 


read.aqs <- function(filename, 
                     level = 2, 
                     time.zone = "UTC", 
                     remove = FALSE) {
  
  # LEVEL 0 ----------------------------------------
  
  data <- read.table(file = filename, # read in data
                     sep = "|", 
                     header = TRUE,
                     colClasses = c(rep("character", 12), 
                                    "numeric", 
                                    rep("character", 13)))
  
  state.code <- data$State.Code # used in level 3
  
  if (remove == TRUE) { # remove columns that contain all same value if remove = TRUE
    
    rcol <- rep(0, ncol(data))
    
    for (i in 1:ncol(data)) {
      
      if (length(unique(data[ , i])) == 1) {
        
        rcol[i] <- 1
        
      }
    
    }
    
    data <- data[ , -which((rcol == 1) & (colnames(data) != "Date"))]

  }
  
  if (level == 0) {
    
    attr(data, "level") <- "level 0" 
    
    return(data)
    
  } 
  
  # LEVEL 1 ----------------------------------------
  
  data$Date.Time <- as.POSIXct(paste(paste(substr(data$Date, 1, 4),
                                           substr(data$Date, 5, 6), 
                                           substr(data$Date, 7, 8), 
                                           sep = "-"), 
                                     data$Start.Time, sep = " "), 
                               format = "%Y-%m-%d %H:%M", 
                               tz = time.zone)
  
  data <- data[ , -c((which(colnames(data) %in% c("Date", # drop unecessary columns
                                                  "Start.Time",
                                                  "RD", 
                                                  "Action.Code", 
                                                  "Null.Data.Code",
                                                  "Sampling.Frequency",
                                                  "Monitor.Protocol..MP..ID", 
                                                  "Alternate.Method.Detectable.Limit", 
                                                  "Uncertainty"))),
                     (which(startsWith(colnames(data), "Qualifier") == TRUE)))]
  
  
  if (level == 1) {
   
    attr(data, "level") <- "level 1"
    
    return(data) 
    
  }
  
  # LEVEL 2 ----------------------------------------
  
  paste.args <- c(data[, -c(which(colnames(data) == "Sample.Value"), # concatenate labels
                            which(colnames(data) == "Date.Time"))], 
                  sep="//")
  
  data$Monitor.ID <- do.call(paste, paste.args)
  
  if (level == 2) {
    
    data <- data[c("Date.Time", "Monitor.ID", "Sample.Value")] 
    
    attr(data, "level") <- "level 2"
    
    return(data)
    
  }
  
  # LEVEL 3 ----------------------------------------
  
  to.upload <- colnames(data)[!(colnames(data) %in% c("Site.ID", 
                                                      "POC", 
                                                      "Sample.Value", 
                                                      "Date.Time",
                                                      "Monitor.ID"))]

  for (i in to.upload) { # load only the label files needed
    
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
  
  paste.args1 <- c(data[, -c(which(colnames(data) == "Sample.Value"), 
                            which(colnames(data)  == "Date.Time"),
                            which(colnames(data)  == "Monitor.ID"))], 
                  sep="//")
  
  data$Monitor.ID <- do.call(paste, paste.args1) # concatenate labels

  data <- data[, c("Date.Time", "Monitor.ID", "Sample.Value")]

  if (level == 3) {
    
    attr(data, "level") <- "level 3"
    
    return(data) 
  
  }
    
  # LEVEL 4 ----------------------------------------
  
  data <- reshape2::dcast(data, # wide -> long format
                          Date.Time ~ Monitor.ID, 
                          value.var = "Sample.Value",
                          na.rm = TRUE,
                          fill = 0)
    
  if (level == 4) {
   
    attr(data, "level") <- "level 4"
    return(data) 

  }

}
  


# ==============================
# get.labels (used in read.aqs)
# ==============================

# takes monitor label file name as argument, returns the file as data frame

get.labels <- function(filename) {
  
  data <- read.table(file = paste("tools.AQS/monitor.labels/", 
                                  filename, 
                                  ".txt", 
                                  sep = ""),
                     sep = "|",
                     header = TRUE,
                     colClasses = c("character", "character"))
  
  return(data)
  
}



# ============
# FOR TESTING
# ============

original <- read.table("tools.AQS/AMP501_1595753-0.txt", sep = "|", header = TRUE,
                       colClasses = c(rep("character", 12), "numeric", 
                                      rep("character", 13)))

test <- read.aqs(filename = "tools.AQS/AMP501_1595753-0.txt", level = 4, remove = T)

original2 <- read.aqs(filename = "C:/Users/loshita/Desktop/AMP501_1594974-0.txt", level = 0, remove = F)


# STILL WORKING ON: just need to test the function on different files







# =========================================================================== 
# Function to load AQS data
# =========================================================================== 



# function will take file name as character string

# header in AQS file must be uncommented before sending through the function
# changed parameter codes to the ones in parameter.csv on AQS site
# there are some missing values in Method.txt

# for this function include reshape library as suggests

# =======
# TO DO: 
# ==========================================================================
# add arguments to RC 
# --- Include the ability to apply the levels to this RC set too

# find a better way to return the data frames (using nested if statements right now)
# ==========================================================================





read.aqs <- function(file, level = 2, time.zone = "UTC", remove = FALSE, RC = "ignore", ...) {
  
  names <- c("RD", "Action.Code", "State.Code", "County.Code", "Site.ID", # column names (for RD only)
             "Parameter", "POC", "Sample.Duration", "Unit", "Method", 
             "Date", "Start.Time", "Sample.Value", "Null.Data.Code", 
             "Sampling.Frequency", "Monitor.Protocol.(MP).ID", "Qualifier - 1",
             "Qualifier - 2", "Qualifier - 3", "Qualifier - 4", "Qualifier - 5",
             "Qualifier - 6", "Qualifier - 7", "Qualifier - 8", "Qualifier - 9",
             "Qualifier - 10", "Alternate.Method.Detectable.Limit", "Uncertainty")
  
  # LEVEL 0 ----------------------------------------
  
  data <- read.table(file = file, # read in data
                     sep = "|",
                     header = FALSE,
                     row.names = NULL,
                     col.names = names,
                     fill = TRUE,
                     colClasses = rep("character", 28),
                     check.names = FALSE,
                     ...)
  
  rc.rows <- grep("RC", x = data$RD, value = FALSE) # indexes of RC rows
  
  if (RC == "include") { # create separate data frame for RC data
    
    rc.data <- data[rc.rows, ]
    
    colnames(rc.data) <- c("RD", "Action.Code", "State.Code", "County.Code", "Site.ID", # apply RC column names
                           "Parameter", "POC", "Unit", "Method", "Year", "Period", 
                           "Number.of.Samples", "Composite.Type", "Sample.Value", 
                           "Monitor.Protocol.(MP).ID", "Qualifier - 1", "Qualifier - 2", 
                           "Qualifier - 3", "Qualifier - 4", "Qualifier - 5", "Qualifier - 6", 
                           "Qualifier - 7", "Qualifier - 8", "Qualifier - 9", "Qualifier - 10",
                           "Alternate.Method.Detectable.Limit", "Uncertainty")
    
    rc.data <- rc.data[ , -which(is.na(colnames(rc.data)))]
    
    rc.data$Sample.Value <- as.numeric(rc.data$Sample.Value)
  
  } 
  
  data <- data[-rc.rows, ]
  data$Sample.Value <- as.numeric(data$Sample.Value)
  state.code <- data$State.Code # used in level 3
  
  if (remove == TRUE) { # remove columns that contain all same value if remove = TRUE
    
    rcol <- rep(0, ncol(data))
    for (i in 1:ncol(data)) {
      if (length(unique(data[ , i])) == 1) {
        rcol[i] <- 1
      }
    }
    data <- data[ , -which((rcol == 1))]
    
  }
  
  if (level == 0) {
    
    attr(data, "level") <- "level 0"
    if (RC == "ignore") {
      return(data)
    } else if (RC == "include") {
      attr(rc.data, "level") <- "level 0"
      return(list(data, rc.data))
    }
    
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
                                                  "Monitor.Protocol.(MP).ID",
                                                  "Alternate.Method.Detectable.Limit",
                                                  "Uncertainty"))),
                     (which(startsWith(colnames(data), "Qualifier") == TRUE)))]
  
  
  if (level == 1) {
    
    attr(data, "level") <- "level 1"
    if (RC == "ignore") {
      return(data)
    } else if (RC == "include") {
      attr(rc.data, "level") <- "level 1"
      return(list(data, rc.data))
    }
    
  }
  
  # LEVEL 2 ----------------------------------------
  
  paste.args <- c(data[, -c(which(colnames(data) == "Sample.Value"), # concatenate labels
                            which(colnames(data) == "Date.Time"))],
                  sep="-")
  
  data$Monitor.ID <- do.call(paste, paste.args)
  
  if (level == 2) {
    
    data <- data[c("Date.Time", "Monitor.ID", "Sample.Value")]
    attr(data, "level") <- "level 2"
    if (RC == "ignore") {
      return(data)
    } else if (RC == "include") {
      attr(rc.data, "level") <- "level 2"
      return(list(data, rc.data))
    }
    
  }
  
  # LEVEL 3 ----------------------------------------
  
  to.use <- colnames(data)[!(colnames(data) %in% c("Site.ID",
                                                   "POC",
                                                   "Sample.Value",
                                                   "Date.Time",
                                                   "Monitor.ID"))]
  
  for (i in to.use) { # load only the label files needed
    
    if (i == "County.Code") {
      
      labelfile <- get("County")
      data$County.Code <- paste(state.code, data$County.Code, sep = "/")
      matches <- match(data$County.Code, labelfile$Code)
      data$County.Code <- labelfile$Region[matches]
      
      
    } else {
      
      labelfile <- get(i)
      var.name <- paste(i, ".Name", sep = "")
      data[[i]] <- labelfile[match(data[[i]], labelfile[[i]]), 2]
      
    }
  }
  
  paste.args1 <- c(data[, -c(which(colnames(data) == "Sample.Value"),
                             which(colnames(data)  == "Date.Time"),
                             which(colnames(data)  == "Monitor.ID"))],
                   sep="-")
  
  data$Monitor.ID <- do.call(paste, paste.args1) # concatenate labels
  
  data <- data[, c("Date.Time", "Monitor.ID", "Sample.Value")]
  
  if (level == 3) {
    
    attr(data, "level") <- "level 3"
    if (RC == "ignore") {
      return(data)
    } else if (RC == "include") {
      attr(rc.data, "level") <- "level 3"
      return(list(data, rc.data))
    }
    
  }
  
  # LEVEL 4 ----------------------------------------
  
  
  data <- reshape2::dcast(data, # wide -> long format
                          Date.Time ~ Monitor.ID,
                          value.var = "Sample.Value",
                          na.rm = TRUE,
                          fill = 0)
  
  if (level == 4) {
    
    attr(data, "level") <- "level 4"
    if (RC == "ignore") {
      return(data)
    } else if (RC == "include") {
      attr(rc.data, "level") <- "level 4"
      return(list(data, rc.data))
    }
    
  }
  
}




# ============
# FOR TESTING
# ============

original <- read.table("tools.AQS/AMP501_1595753-0.txt", sep = "|", header = TRUE,
                       colClasses = c(rep("character", 12), "numeric", 
                                      rep("character", 13)))

test <- read.aqs(file = "testdata.txt", level = 4, RC = "include")

testrd <- test[[1]]
testrc <- test[[2]]




# read in label files so function will work: 

Method <- read.table("tools.AQS/monitor.labels/Method.txt", 
                     header = TRUE,
                     sep = "|",
                     colClasses = c("character", "character"))
County <- read.table("tools.AQS/monitor.labels/County.txt", 
                     header = TRUE,
                     sep = "|",
                     colClasses = c("character", "character"))
Parameter<- read.table("tools.AQS/monitor.labels/Parameter.txt", 
                     header = TRUE,
                     sep = "|",
                     colClasses = c("character", "character"))
Sample.Duration <- read.table("tools.AQS/monitor.labels/Sample.Duration.txt", 
                     header = TRUE,
                     sep = "|",
                     colClasses = c("character", "character"))
State.Code <- read.table("tools.AQS/monitor.labels/State.Code.txt", 
                     header = TRUE,
                     sep = "|",
                     colClasses = c("character", "character"))
Unit <- read.table("tools.AQS/monitor.labels/Unit.txt", 
                     header = TRUE,
                     sep = "|",
                     colClasses = c("character", "character"))







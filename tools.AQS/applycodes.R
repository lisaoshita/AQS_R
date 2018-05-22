# ===============================================
# Function to translate concatenated monitor IDs
# ===============================================

# works on level 0, 1, 2 data frames 
# if level 0 is input, it translates the null codes and qualifiers

# NEED TO FIND LABELS FOR THE NULL CODES AND QUALIFIERS

# leaves Site ID as number

# function needs the state variable to match County.Code with correct county (duplicate county.codes)

# for testing
level0 <- read.aqs(filename = "tools.AQS/AMP501_1595753-0.txt", level = 0, remove = F)
level1 <- read.aqs(filename = "tools.AQS/AMP501_1595753-0.txt", level = 1, remove = F)
level2 <- read.aqs(filename = "tools.AQS/AMP501_1595753-0.txt", level = 2, remove = F)


apply.codes <- function(data) {
  
  level <- attributes(data)$level
  
  stopifnot((level != "level 3") | (level != "level 4")) # stop function if incorrect levels
  
  if ((level == "level 0") or (level == "level 1")) {
    
    # apply monitor names similar to how it's done in read.aqs
    to.upload <- colnames(data)[!(colnames(data) %in% c("RD", 
                                                        "Action.Code",
                                                        "Site.ID", 
                                                        "POC", 
                                                        "Date",
                                                        "Start.Time",
                                                        "Sample.Value", 
                                                        "Monitor.ID"))]
    to.upload <- to.upload[1:6] # remove this when find null and qualifier code
    print(to.upload)
    
    return(data) 
    
  } if (level == "level 2") {
   
    print("yas") 
  }

}


testing <- apply.codes(level0) # FUNCTION NOT WORKING CORRECTLY!!!!!

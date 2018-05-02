# ===================================================
# Script to get list of subscribers
# ===================================================

# packages used
library(rJava)
library(xlsx)

# DON'T NEED THIS SCRIPT - PYTHON SCRIPT DOES THIS ALL


# ---------------------------------------------------
# Function to load excel file with subscriber numbers
#     - takes filename argument as character string 
#     - returns phone numbers as character string
# note: renamed subscriber list file to make it easier to load
# ---------------------------------------------------




get_numbers <- function(filename) {
  
  # load file with subscribers
  file <- read.xlsx(file = filename,
                    sheetIndex = 2,
                    header = T,
                    colIndex = c(1, 3),
                    colClasses = c("character", "character"))
  
  # convert DUST to all lower case + remove white spaces
  file$DUST <- stringr::str_to_lower(file$DUST)
  file$DUST <- stringr::str_trim(file$DUST, side = "both")
  
  nums <- as.character(file$PHONE[file$DUST == "yes"])
  
  nums <- nums[nchar(nums) >= 10]
  
  nums[nchar(nums) < 11] <- paste("1", 
                                  nums[nchar(nums) < 11], 
                                  sep = "")
  
  front <- "{\"binding_type\":"
  nums <- paste("'+", trimws(nums, which = "both"), "'", sep = "")
  end <- "}'"
  
  numsdf <- data.frame(Numbers = paste(front, nums, end, sep = ""))
  
  write.table(numsdf, 
              file = "H:/TECH/Lisa/R/numbers.csv",
              sep = ",",
              col.names = T,
              row.name = F)
}




# change the way puts the string together
# save this to a csv
# run the python file from r function
#   python function will upload csv


get_numbers(filename = "H:/TECH/Lisa/R/subscriber.list6.15.17.xlsx")



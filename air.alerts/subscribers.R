# ===================================================
# Script to get list of subscribers
# ===================================================

# packages used
library(rJava)
library(xlsx)


# ---------------------------------------------------
# Function to load excel file with subscriber numbers
#     - takes filename as string 
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
  
  numbers <- as.character(file$PHONE[file$DUST == "yes"])
  
  return(numbers)
  
}


phone_nums <- get_numbers(filename = "H:/TECH/Lisa/R/subscriber.list6.15.17.xlsx")



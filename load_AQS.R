# ======================================= 
# Function to load AQS data
# ======================================= 

# function will take file name as a character string
# file must be in working directory

read.aqs <- function(filename) {
  data <- read.table(filename, sep = "|", header = TRUE)
  return(data)
}

data <- read.aqs(filename = "AMP501_1595753-0.txt")



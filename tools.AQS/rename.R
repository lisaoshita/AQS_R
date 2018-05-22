# =================================
# Function to rename monitor names
# =================================

# Interactively rename/translate monitor names
# define new class/attributes to keep track of monitor information? - if rename monitor to something 
# simple - it's site/pollutant... are still stored in attributes somewhere


# experimenting with readline()

# function renames the columns of a data frame
rename <- function(data) {
  
  monitors <- colnames(data[, -1])
  new.names <- rep(NA, length(monitors))
  
  for (i in 1:length(monitors)) {
    new.names[i] <- readline(paste("Rename monitor: \n", monitors[i], "\n", sep = " ")) # fix the way this prints
  }
  
  colnames(data)<- c("Date.Time", new.names)
  return(data)

}

# monitor names
monitors <- colnames(test[, -1])

monitors_m <- str_split(monitors, pattern = "//", simplify = T)


# split up and assign each value as attribute to the monitor??

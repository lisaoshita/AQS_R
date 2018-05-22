# experimenting with readline()


# this function renames the columns of a data frame
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

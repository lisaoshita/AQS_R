# =========================================================================== 
# setting up files for monitor labels
# =========================================================================== 

library(dplyr)
library(magrittr)
library(stringr)


# -----------------------------------------------------------
# states_counties file
# -----------------------------------------------------------

regions <- read.csv("C:/Users/loshita/Desktop/Projects/states_and_counties.csv",
                    header = TRUE,
                    stringsAsFactors = FALSE)

regions$state_code <- as.character(regions$state_code)
regions$county_code <- as.character(regions$county_code)

# adding leading 0s 
regions$state_code <- str_pad(regions$state_code,
                              width = 2,
                              pad = "0",
                              side = "left")
regions$county_code <- str_pad(regions$county_code,
                               width = 3,
                               pad = "0",
                               side = "left")
# concatenate state and county code
regions$code <- paste(regions$state_code, 
                      regions$county_code,
                      sep = "/")

regions$county_name <- trimws(regions$county_name, which = "both")

regions$county_name <- str_replace_all(regions$county_name,
                                       pattern = " ",
                                       replacement = "_")
regions$region <- paste(regions$state_abbr,
                        regions$county_name,
                        sep = "/")
regions <- regions %>% select(code, region)

write.table(regions, "load.AQS/monitor.labels/regions.txt", sep = "|", col.names = T, row.names = F)

# -----------------------------------------------------------
# Parameters
# -----------------------------------------------------------

parameters <- read.csv("H:/TECH/Lisa/R/AQS_R/monitor.labels/parameters.csv",
                    header = TRUE,
                    stringsAsFactors = FALSE)
write.table(parameters, "parameters.txt", sep = "|", col.names = T, row.names = F)


# -----------------------------------------------------------
# Methods
# -----------------------------------------------------------

methods <- read.csv("H:/TECH/Lisa/R/AQS_R/monitor.labels/methods.csv",
                    header = TRUE,
                    stringsAsFactors = FALSE)
methods$Method <- str_pad(methods$Method,
                          width = 3,
                          pad = "0",
                          side = "left")

write.table(methods, "methods.txt", sep = "|", col.names = T, row.names = F)



# -----------------------------------------------------------
# durations file
# -----------------------------------------------------------

durations <- read.csv("H:/TECH/Lisa/R/AQS_R/monitor.labels/durations.csv",
                      header = TRUE,
                      stringsAsFactors = FALSE)

write.table(durations, "durations.txt", sep = "|", col.names = T, row.names = F)


# -----------------------------------------------------------
# units file
# -----------------------------------------------------------

units <- read.csv("H:/TECH/Lisa/R/AQS_R/monitor.labels/units.csv",
                      header = TRUE,
                      stringsAsFactors = FALSE)

units$Unit <- as.character(units$Unit)
units$Unit <- stringr::str_pad(units$Unit,
                           width = 3,
                           pad = "0",
                           side = "left")

colnames(units) <- c("Unit", "unit_label")

units$unit_label <- str_to_lower(units$unit_label)
units$unit_label <- str_replace_all(units$unit_label,
                                    pattern = " ",
                                    replacement = "_")

write.table(units, "units.txt", sep = "|", col.names = T, row.names = F)















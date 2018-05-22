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
regions$state_abbr <- str_replace_all(regions$state_abbr, pattern = " ", replacement = "")
regions$county_name <- str_replace_all(regions$county_name, pattern = " ", replacement = "")

State.Code <- regions[, 1:2]
State.Code$state_code <- str_pad(State.Code$state_code,
                                 width = 2,
                                 pad = "0",
                                 side = "left")
write.table(State.Code, "load.AQS/monitor.labels/State.Code.txt", sep = "|", col.names = T, row.names = F)


County.Code <- regions[, 3:4]
County.Code$county_code <- str_pad(County.Code$county_code,
                                   width = 3,
                                   pad = "0",
                                   side = "left")
write.table(County.Code, "load.AQS/monitor.labels/County.Code.txt", sep = "|", col.names = T, row.names = F)

# reducing State.Code.txt

states <- read.table("load.AQS/monitor.labels/State.Code.txt", sep = "|", header = T, stringsAsFactors = F)

states <- states %>% distinct(State.Code, State.Name)

write.table(states, "load.AQS/monitor.labels/State.Code.txt", sep = "|", col.names = T, row.names = F)


# -----------------------------------------------------------
# Parameters
# -----------------------------------------------------------

methods_criteria <- read.csv("C:/Users/loshita/Desktop/Projects/methods_criteria.csv",
                    header = TRUE,
                    stringsAsFactors = FALSE)

# parameters <- methods_criteria %>% select(Parameter.Code, Parameter) %>% distinct()
# 
# # reformatting Parameter.txt
# 
# parameters <- read.table("load.AQS/monitor.labels/Parameter.txt",
#                          header = T,
#                          sep = "|",
#                          stringsAsFactors = F)
# parameters$Parameter.Name <- str_replace_all(parameters$Parameter.Name, pattern = " ", replacement = "-")
# 
# write.table(parameters, "load.AQS/monitor.labels/Parameter.txt", 
#             sep = "|", 
#             col.names = T,
#             row.names = T)

parameters <- read.csv("C:/Users/loshita/Desktop/Projects/parameters.csv", header = T, stringsAsFactors = F)

parameters <- parameters %>% select(Parameter.Code, Parameter)
parameters$Parameter <- str_replace_all(parameters$Parameter,
                                        pattern = " ",
                                        replacement = "_")
write.table(parameters, "load.AQS/monitor.labels/Parameter.txt", sep = "|", col.names = T, row.names = F)

# -----------------------------------------------------------
# Methods
# -----------------------------------------------------------

methods <- read.table("load.AQS/monitor.labels/Method.txt",
                      header = T,
                      sep = "|",
                      stringsAsFactors = F)
methods$Method.Label <- str_replace_all(methods$Method.Label, pattern = " ", replacement = "-")

methods <- methods_criteria %>% select(Method.Code, Equivalent.Method)
methods$Method <- as.character(Method)
methods$Method <- str_pad(methods$Method,
                          width = 3,
                          pad = "0",
                          side = "left")

write.table(methods, "load.AQS/monitor.labels/Method.txt", sep = "|", col.names = T, row.names = F)



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















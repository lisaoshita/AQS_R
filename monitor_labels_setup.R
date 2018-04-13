# =========================================================================== 
# setting up files for monitor labels
# =========================================================================== 

library(dplyr)
library(magrittr)
library(stringr)


# -----------------------------------------------------------
# states_counties file
# -----------------------------------------------------------

regions <- read.csv("H:/TECH/Lisa/R/AQS_R/monitor_labels/states_and_counties.csv",
                    header = TRUE,
                    stringsAsFactors = FALSE)
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
                      sep = "-")

regions$county_name <- trimws(regions$county_name, which = "both")

regions$county_name <- str_replace_all(regions$county_name,
                                       pattern = " ",
                                       replacement = "_")
regions$region <- paste(regions$state_abbr,
                        regions$county_name,
                        sep = "-")
regions <- regions %>% select(code, region)

# write.csv(regions, file = "regions.csv")

# -----------------------------------------------------------
# methods_criteria file
# -----------------------------------------------------------

methods <- read.csv("H:/TECH/Lisa/R/AQS_R/monitor_labels/methods_criteria.csv",
                    header = TRUE,
                    stringsAsFactors = FALSE)

# for parameter code
parameter <- data.frame(Parameter = unique(methods$Parameter.Code),
                        parameter_label = unique(methods$Parameter))

parameter$Parameter <- as.character(parameter$Parameter)

parameter$parameter_label <- str_to_lower(parameter$parameter_label)
parameter$parameter_label <- str_replace_all(parameter$parameter_label, 
                                             pattern = " ",
                                             replacement = "_")

write.csv(parameter, file = "parameter.csv")

# for method code
methods <- methods %>% select(Method.Code, Equivalent.Method)

colnames(methods) <- c("Method", "method_label")
methods$Method <- as.character(methods$Method)

methods$method_label <- str_to_lower(methods$method_label)
methods$method_label <- str_replace_all(methods$method_label,
                                        pattern = " ",
                                        replacement = "_")

write.csv(methods, file = "methods.csv")


# -----------------------------------------------------------
# durations file
# -----------------------------------------------------------

durations <- read.csv("H:/TECH/Lisa/R/AQS_R/monitor_labels/durations.csv",
                      header = TRUE,
                      stringsAsFactors = FALSE)

durations <- durations %>% select(Duration.Code, 
                                  Duration.Description)

colnames(durations)[1] <- "Sample.Duration"

durations$Duration.Description <- str_to_lower(durations$Duration.Description)

durations$Duration.Description <- str_replace_all(durations$Duration.Description,
                                                  pattern = " ",
                                                  replacement = "_")

write.csv(durations, file = "durations.csv")


# -----------------------------------------------------------
# units file
# -----------------------------------------------------------

units <- read.csv("H:/TECH/Lisa/R/AQS_R/monitor_labels/units.csv",
                      header = TRUE,
                      stringsAsFactors = FALSE)

units$Unit.Code <- str_pad(units$Unit.Code,
                           width = 3,
                           pad = "0",
                           side = "left")

colnames(units) <- c("Unit", "unit_label")

units$unit_label <- str_to_lower(units$unit_label)
units$unit_label <- str_replace_all(units$unit_label,
                                    pattern = " ",
                                    replacement = "_")

write.csv(units, file = "units.csv")















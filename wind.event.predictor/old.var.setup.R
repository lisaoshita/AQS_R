# =======================================================
# Code to set up the old version of training + test sets 
# =======================================================

# ======= LOAD DATA =======================================================================
s1.cdf.data <- read_csv("H:/TECH/Lisa/R/apcd.r/wind.event.predictor/forLisa.csv",
                        col_types = list(date = "c", ws.cdf = "n", wd.cdf = "n",
                                         pm10.cdf = "n", pm10.oso = "n", wd.s1 = "n",
                                         ws.s1 = "n", year = "n")) # contains data from 2011 - 2017

s1.cdf.data <- s1.cdf.data %>%
  mutate(date = parse_date_time(date, "Ymd HMS"))

# contains cdf and S1 data up to 2014
cdf.master <- read_csv("H:/TECH/Lisa/R/apcd.r/wind.event.predictor/cdf.master.csv",
                       col_types = list(date = "c", ws = "n", wd = "n", pm25 = "n",
                                        pm10 = "n", u = "n", v = "n", year = "n", 
                                        precip = "n", s.rad = "n", a.temp = "n",
                                        rh = "n", dp = "n", s.temp = "n", height = "n",
                                        temp850 = "n", ws.max = "n", wd.max = "n",
                                        u.max = "n", v.max = "n", time = "n", dow = "n",
                                        u.s1 = "n", v.s1 = "n", u.max.s1 = "n", v.max.s1 = "n"))

cdf.master$date <- date(cdf.master$date)

# contains cdf and S1 data from 2014 - 2017
cdf.master2 <- read_csv("H:/TECH/Lisa/R/apcd.r/wind.event.predictor/cdf.master.update.csv",
                        col_types = list(date = "c", ws = "n", wd = "n", pm25 = "n", pm10 = "n",
                                         u = "n", v = "n", year = "n", precip = "n",
                                         s.rad = "n", a.temp = "n", rh = "n", dp = "n",
                                         s.temp = "n", height = "n", temp850 = "n", ws.max = "n",
                                         wd.max = "n", u.max = "n", v.max = "n", time = "n",
                                         dow = "n", u.s1 = "n", v.s1 = "n", u.max.s1 = "n",
                                         v.max.s1 = "n"))

cdf.master2$date <- date(cdf.master2$date)

# ======= TRAINING SET =====================================================================

# train on years before 2015
training <- s1.cdf.data %>%
  mutate(year = lubridate::year(date)) %>%
  mutate(date.only = lubridate::date(date)) %>%
  filter(year < 2015)

# finding wd and ws values at CDF and S1 that correspond with high pm10 concentrations
# using openair package
# ====
# CDF 
# ====
cdf.training <- training %>%
  select(date, year, ws.cdf, wd.cdf, pm10.cdf)
colnames(cdf.training) <- c("date", "year", "ws", "wd", "pm10")

cdf.clust <- polarCluster(cdf.training, 
                          pollutant = "pm10",
                          x = "ws",
                          wd = "wd",
                          n.clusters = 2) # cluster 2 is high pm10 

cdf.clust$data %>%
  filter(cluster == 2) %>%
  summarize(min_wd = min(wd),
            max_wd = max(wd),
            min_ws = min(ws),
            max_ws = max(ws)) # CRITERIA: wd between 288 - 320, ws between 9.2 - 20.7

# ===
# S1 
# ===
s1.training <- training %>%
  select(date, year, wd.s1, ws.s1, pm10.cdf)
colnames(s1.training) <- c("date", "year", "wd", "ws", "pm10")

s1.clust <- polarCluster(s1.training,
                         pollutant = "pm10",
                         x = "ws",
                         wd = "wd",
                         n.clusters = 2)

s1.clust$data %>%
  filter(cluster == 2) %>%
  summarize(min_wd = min(wd),
            max_wd = max(wd),
            min_ws = min(ws),
            max_ws = max(ws)) # CRITERIA: high pm10 if wd between 281 - 306, ws between 8.88 - 16.11

# WD and WS variable set up 
# - creating variables indicating if wd or ws fall within the ranges found above
# - maximum ws and wd per day for both S1 and CDF
# - hour of the day that WS and WD were at a maximum
train11 <- training %>%
  group_by(date.only) %>%
  summarize(max.ws.cdf = max(ws.cdf, na.rm = TRUE),
            max.wd.cdf = max(wd.cdf, na.rm = TRUE),
            max.ws.s1 = max(ws.s1, na.rm = TRUE),
            max.wd.s1 = max(wd.s1, na.rm = TRUE),
            hour.max.wd.s1 = ifelse(length(which.max(wd.s1)) == 0, NA, which.max(wd.s1) - 1),
            hour.max.ws.s1 = ifelse(length(which.max(ws.s1)) == 0, NA, which.max(ws.s1) - 1),
            hour.max.wd.cdf = ifelse(length(which.max(wd.cdf)) == 0, NA, which.max(wd.cdf) - 1),
            hour.max.ws.cdf = ifelse(length(which.max(ws.cdf)) == 0, NA, which.max(ws.cdf) - 1),
            ws.in.range.cdf = sum((ws.cdf > 9), na.rm = TRUE),
            wd.in.range.cdf = sum((wd.cdf > 288) & (wd.cdf < 320), na.rm = TRUE),
            wd.in.range.s1 = sum((wd.s1 > 281) & (wd.s1 < 306), na.rm = TRUE),
            ws.in.range.s1 = sum((ws.s1 > 8), na.rm = TRUE)) %>%
  mutate(max.ws.cdf = ifelse(max.ws.cdf == -Inf, NA, max.ws.cdf)) %>%
  mutate(max.wd.cdf = ifelse(max.wd.cdf == -Inf, NA, max.wd.cdf)) %>%
  mutate(max.ws.s1 = ifelse(max.ws.s1 == -Inf, NA, max.ws.s1)) %>%
  mutate(max.wd.s1 = ifelse(max.wd.s1 == -Inf, NA, max.wd.s1))

# computing pm10 avg 24 hr concentration
pm10.averages11 <- training %>%
  group_by(date.only) %>%
  summarize(pm10.ave = mean(pm10.cdf, na.rm = TRUE)) %>%
  mutate(did.exceed = ifelse(pm10.ave >= 50, "yes", "no"))

colnames(train1)[1] <- "date"

# merge train1 with other columns in cdf.master
train111 <- train11 %>%
  mutate(did.exceed = pm10.averages$did.exceed) %>%
  left_join(cdf.master, by = "date") %>%
  mutate(month = month(date)) %>%
  mutate(day.of.month = day(date)) %>%
  select(did.exceed, ws.in.range.cdf, wd.in.range.cdf, wd.in.range.s1, 
         ws.in.range.s1, max.ws.cdf, max.wd.cdf, max.ws.s1, max.wd.s1,
         hour.max.wd.s1, hour.max.ws.s1, hour.max.wd.cdf, hour.max.wd.cdf, 
         precip, s.rad, a.temp, rh, dp, s.temp, height, temp850, month, day.of.month)

# examining missing data w/ Missingness map 
Amelia::missmap(train11) # only 3% of the training data is missing - ok to omit these rows
train1 <- na.omit(train1) 

train1$did.exceed <- as.factor(train1$did.exceed)

# ======= TEST SET =====================================================================

# test on years after 2015
testing <- s1.cdf.data %>%
  mutate(year = lubridate::year(date)) %>%
  mutate(date.only = lubridate::date(date)) %>%
  filter(year >= 2015)

# creating variables
test <- testing %>%
  group_by(date.only) %>%
  summarize(max.ws.cdf = max(ws.cdf, na.rm = TRUE),
            max.wd.cdf = max(wd.cdf, na.rm = TRUE),
            max.ws.s1 = max(ws.s1, na.rm = TRUE),
            max.wd.s1 = max(wd.s1, na.rm = TRUE),
            hour.max.wd.s1 = ifelse(length(which.max(wd.s1)) == 0, NA, which.max(wd.s1) - 1),
            hour.max.ws.s1 = ifelse(length(which.max(ws.s1)) == 0, NA, which.max(ws.s1) - 1),
            hour.max.wd.cdf = ifelse(length(which.max(wd.cdf)) == 0, NA, which.max(wd.cdf) - 1),
            hour.max.ws.cdf = ifelse(length(which.max(ws.cdf)) == 0, NA, which.max(ws.cdf) - 1),
            ws.in.range.cdf = sum((ws.cdf > 9), na.rm = TRUE),
            wd.in.range.cdf = sum((wd.cdf > 288) & (wd.cdf < 320), na.rm = TRUE),
            wd.in.range.s1 = sum((wd.s1 > 281) & (wd.s1 < 306), na.rm = TRUE),
            ws.in.range.s1 = sum((ws.s1 > 8), na.rm = TRUE)) %>%
  mutate(max.ws.cdf = ifelse(max.ws.cdf == -Inf, NA, max.ws.cdf)) %>%
  mutate(max.wd.cdf = ifelse(max.wd.cdf == -Inf, NA, max.wd.cdf)) %>%
  mutate(max.ws.s1 = ifelse(max.ws.s1 == -Inf, NA, max.ws.s1)) %>%
  mutate(max.wd.s1 = ifelse(max.wd.s1 == -Inf, NA, max.wd.s1))

colnames(test)[1] <- "date"

# computing 24 hour average pm10 concentration
pm10.averages.test <- testing %>%
  group_by(date.only) %>%
  summarize(pm10.ave = mean(pm10.cdf, na.rm = TRUE)) %>%
  mutate(did.exceed = ifelse(pm10.ave >= 50, "yes", "no"))

# merge test with other columns in cdf.master2
test <- test %>%
  mutate(did.exceed = pm10.averages.test$did.exceed) %>%
  left_join(cdf.master2, by = "date") %>%
  mutate(month = month(date)) %>%
  mutate(day.of.month = day(date)) %>%
  select(did.exceed, ws.in.range.cdf, wd.in.range.cdf, wd.in.range.s1, 
         ws.in.range.s1, max.ws.cdf, max.wd.cdf, max.ws.s1, max.wd.s1,
         hour.max.wd.s1, hour.max.ws.s1, hour.max.wd.cdf, hour.max.wd.cdf, 
         precip, s.rad, a.temp, rh, dp, s.temp, height, temp850, month, day.of.month)

# assess rows with missing data
Amelia::missmap(test) # only 2% of the training data is missing - ok to omit these rows
test <- na.omit(test) 

test$did.exceed <- as.factor(test$did.exceed)

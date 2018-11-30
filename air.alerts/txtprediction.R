
## See explanation at bottom

## read in, format data
d <- read.csv("air.alerts/cdf.csv", skip=2)
d <- d[, c(1,5)] # for current "cdf.csv" file, which likely is not what was used orignally
names(d) <- c("date", "pm")
summary(d)
which(d$pm > 800)
d[which(d$pm > 800), "pm"] <- NA

d$date <- as.POSIXct(d$date, tz = "UTC", format = "%d-%b-%Y %H:%M")
d$day <- format(d$date, format = "%Y-%m-%d")

summary(d)

## split into list
dd <- split(d, d$day)

## remove days with less than 75% completeness
id <- sapply(dd, function(a) sum(is.na(a$pm)))
dd <- dd[id < 7]
rm(id)


###functions for testing accuracy

predict <- function(data, trigger) {
  sapply(data, function(a) sum(a$pm > trigger, na.rm = TRUE) > 0) # returns a boolean for if a text will be triggered at all
}

means <- function(data) {
  sapply(data, function(a) mean(a$pm, na.rm = TRUE))
}

acc <- function(pred, true){
  sum(pred == true) / length(true)
}

fpr <- function(pred, true){
  sum(pred == TRUE & true == FALSE) / length(true)
}

fpr2 <- function(pred, true){
  sum(pred == TRUE & true == FALSE) / sum(pred == TRUE)
}

fnr <- function(pred, true){
  sum(pred == FALSE & true == TRUE) / length(true)
}

fnr2 <- function(pred, true){
  sum(pred == FALSE & true == TRUE) / sum(pred == FALSE)
}

test.trigger <- function(data, trigger, thresh){
  
  true <- means(data) > thresh # if the 24 hour average pm10 is greater than the threshold return TRUE
  pred <- predict(data, trigger) # return true if algorithm will send out a text
  accuracy <- acc(pred, true) # out of all days, how many times was the algorithm correct
  false.pos <- fpr(pred, true) 
  false.pos2 <- fpr2(pred, true)
  false.neg <- fnr(pred, true)
  false.neg2 <- fnr2(pred, true)
  true.rate <- sum(true)/length(true)
  
  ret <- c(thresh, true.rate, trigger, accuracy, false.pos, false.pos2, false.neg, false.neg2)
  return(ret)
}


# run it! 
thresh <- 50
trigs <- c(seq(50, 400, by = 10), 160:180)
results <- test.trigger(dd, 40, thresh)

for(i in 1:length(trigs)){
  x <- test.trigger(dd, trigs[i], thresh)
  results <- rbind(results, x)
  rm(x)
} ; rm(i)

results <- as.data.frame(results, row.names = 1:nrow(results))
names(results) <- c("threshold", "true.rate", "trigger", "accuracy", 
                    "false.pos", "false.pos2", "false.neg", "false.neg2")
results <- results[order(results$trigger), ]

# plot it!
plot(results$trigger, results$accuracy, type = "l", ylim = c(0,1),
     main = paste("Target 24-hr average:", thresh, "ug/m3"),
     xlab = "Hourly concentration trigger (ug/m3)",
     ylab = "%",
     yaxt = "n")
axis(2, at = (0:5)/5, labels = (0:5)*20 )
lines(results$trigger, results$false.pos2, col=2)
lines(results$trigger, results$false.pos, col=2, lty=2)
lines(results$trigger, results$false.neg2, col=3)
lines(results$trigger, results$false.neg, col=3, lty=2)

legend("right", 
       legend = c("Accuracy", "False Pos Rate", "Scaled False Pos Rate",
                           "False Neg Rate", "Scaled False Neg Rate"),
       lty = c(1, 2, 1, 2, 1),
       col = c(1, 2, 2, 3, 3),
       bty = "n")

abline(v = results$trigger[which(results$accuracy == max(results$accuracy))[1]],
       lty = 2)
axis(1, 
     at = results$trigger[which(results$accuracy == max(results$accuracy))[1]])

### when is first SMS sent?

d$txt <- d$pm > 175
d$hour <- as.numeric(format(d$date, "%H"))

ddd <- split(d, d$day)

min.hour <- function(a) {
  a <- a[!is.na(a$txt), ]
  if(sum(a$txt) == 0) {
    ret <- NA
  } else {
    ret <- min(a$hour[a$txt == TRUE], na.rm = TRUE)
  }
  return(ret)
}

first.hour <- sapply(ddd, min.hour)
mean(first.hour, na.rm = T)


# My to-do from Friday afternoon's meeting was to look at the data and come up with 
# criteria for automatically sending out the AirAware Early Action Text Alert (or 
# whatever we are calling it). I looked all PM10 from CDF from 2013 thru Friday, 
# and my recommendation is to use an hourly CDF PM10 value of 170 ug/m3 as the 
# threshold for sending out the alert. 

# This threshold is based on trying to predict daily 24-hr averages that exceed the 
# state standard of 50 ug/m3. We could base the hourly threshold off of a different 
# "target" 24-hr average, but the state standard seems easiest to justify. Any alternative 
# is going to be to somewhat arbitrary.

# I looked at various hourly thresholds and 170 gives the best overall accuracy for 
# predicting whether the day's 24-hour average will exceed 50. This threshold is 93% 
# accurate, meaning 93% of the time, either no alert goes out and the day's 24-hr 
# average is indeed less than 50, OR one or more alerts do go out and the 24-hr average 
# is above 50. 7% of the time it gets it wrong: either 1 (or more) alerts go out but 
# the 24-hr average is less than the standard, or no alerts go out but the daily average 
# winds up exceeding the standard.

# Looking more closely at when it gets it wrong, 15% of the days when an alerts goes out 
# are false positives, i.e. the 24-hr average ends up being less than the standard. 5% 
# of days when no alert goes out are false negatives, in which the standard is exceeded 
# but no alert was sent. So the algorithm has a health protective bias: It errs on the 
# side of sending an alert and incurring a false positive.

# To illustrate how this would have worked had it been in place this year, I have attached 
# an annotated print out of CDF PM10 data for March 2017 thru today. I have highlighted in 
# yellow when press releases are triggered under the current system--the first hour each day 
# when PM10 exceeds 300. Then I have boxed in red all hours above the proposed threshold of 
# 170--this is when an alert would have been sent under this proposal. Finally, I have added 
# a column showing whether the day was a false positive, a false negative, or an accurate 
# prediction of an exceedence. 

# I think the follow things are notable:
# •The proposed threshold of 170 typically gives 1 to 3 hours earlier notice than the 300 
# threshold, but not always: there are several days when concentrations jump from <100 to >300.
# •Over these ~3.5 months, there would have been 6 false positives (alert sent but 24-hour 
# standard not exceeded) and 1 false negative (no alert, but 24-hour standard exceeded.)
# •There are several exceedence days that would have generated an alert with 170 threshold 
# but did not generate a press release with the 300 threshold.
# •Somebody subscribed to the Alerts would have received about 170 text messages during 
# this period of time.

# So to recap, my recommendation is to use the state standard as the "target" and use 170 
# as the trigger for sending out an alert. 170 has the highest overall accuracy, and when it 
# errs it errs in a health protective manner. 

# If 170 alerts in 3.5 months are too many, we could consider a higher "target" 24-hour average. 
# (FWIW, our current trigger of 300 ug/m3 corresponds to a 24-hour average of about 70 ug/m3). 

# Alternatively, we could move the trigger to a higher value, at the expense of overall accuracy. 
# This would decrease the false positive rate (and thus the number of emails sent) while 
# increasing the false negative rate. Overall accuracy takes a hit, but a surprisingly small one. 
# For example, increasing the trigger/threshold from 170 to 200 yields only a slight decrease in 
# overall accuracy (92.6% vs 92.8%) but the false positive rate drops from 15 to 8% while the 
# false negative rate jumps from 5 to 7%, and the number of alerts sent would drop from about 170 
# to about 130. This higher threshold would, however, tend to give less warning before the really 
# high concentrations start.

# Another alternative is to only send out one alert per day. So once the hourly threshold is 
# exceeded and an alert is triggered, no additional alerts are sent. Unfortunately this cannot 
# be implemented on the data side--the email rules in AirVision simply aren't flexible enough. 
# But maybe there is a way on Outlook-side to specify only one email per day.

# Let me know if there are questions. I am happy to explore other ideas for triggering the alert.

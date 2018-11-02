
library(dplyr)
library(sp)

# functions to convert polar coordinates to cartesian:
make.x <- function(ws, wd){
  ws*cos((90-wd)*pi/180)
}

make.y <- function(ws, wd){
  ws*sin((90-wd)*pi/180)
}

s1.training <- training %>%
  select(date, year, wd.s1, ws.s1, pm10.cdf)
colnames(s1.training) <- c("date", "year", "wd", "ws", "pm10")

s1.clust <- polarCluster(s1.training,
                         pollutant = "pm10",
                         x = "ws",
                         wd = "wd",
                         n.clusters = 2)


# get cluster of high PM and create cartersian coordinates:
s1.clust$data %>% 
  filter(cluster == 2) %>%
  mutate(x = make.x(ws, wd)) %>%
  mutate(y = make.y(ws, wd)) -> s1.range

# check (looks reasoable):
summary(s1.range)       
plot(s1.range$x, s1.range$y,  #compare with print(s1.clust) 
     ylim = c(-15, 15), xlim = c(-15, 15),
     asp = 1, pch = 16, cex = 0.5)

# get convex hull
chull.index <- chull(s1.range$x, s1.range$y)
chull.index <- c(chull.index, chull.index[1])
s1.range.chull <- s1.range[chull.index, c("x", "y")]

# check --> looks good!
lines(s1.range.chull$x, s1.range.chull$y,
      col = "red") # add to current plot

# function to see if a point falls in the s1 range:
wind.in.range <- function(ws, wd, range){
  # assumes range is a two column df with "x" and "y"
  
  # assumes ws and wd in usual format, 
  # so must convert to cartesian coords.
  # define these functions again, in case they are not 
  # in environment:
  
  make.x <- function(ws, wd){
    ws*cos((90-wd)*pi/180)
  }
  
  make.y <- function(ws, wd){
    ws*sin((90-wd)*pi/180)
  }
  
  xs <- make.x(ws, wd)
  ys <- make.y(ws, wd)
  
  # test if in range
  res <- point.in.polygon(xs, ys, range$x, range$y)
  
  # return 0 if outside, 1 if inside or on edge, NA if ws or wd is missing
  res <- ifelse(res == 0, 0, 1) # see ?point.in.polygon
  res[is.na(ws) | is.na(wd)] <- NA # preserve NA's
  return(res) 
}

## test function: does it 'predict' if ws/wd pair is in high PM10 cluster?

preds <- wind.in.range(s1.clust$data$ws, s1.clust$data$wd, s1.range.chull)
table(s1.clust$data$cluster, preds)

# hmmm... 52 observation from cluster 1 are predicted to be in cluster 2
s1.clust$data[preds == 1 & s1.clust$data$cluster == 1, ]
# these all appear to be on the edge, so OK if predicted to be in cluster 2
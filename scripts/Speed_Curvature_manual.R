# roam/dwell test using wormlab data - adds length info - need to add omega and other data
packages = c("ggplot2","dplyr","tidyr","reshape2","viridis","plotly", "magrittr")
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

WL.roam.data.edit <- function(bin.length, frame.rate, num.tracks) {

  #### import data ### - use  readr to improve speed
  message("Choose position data")
  print(system.time(position <- read.csv(file.choose(), skip=4)))
  message("Choose Direction data")
  direction <- read.csv(file.choose(), skip=4)
  message("Choose speed data")
  speed <- read.csv(file.choose(), skip=4)
  message("Choose length data")
  length <- read.csv(file.choose(), skip=4)

  ####Setting up parameters####
  if(missing(num.tracks)) {
    num.tracks <- length(speed) - 2
  } else {
    num.tracks = num.tracks
  }
  vid.length <- max(position$Frame)
  bin.length <- bin.length # bin length in s
  frame.rate <- frame.rate # usually 2 or 3 fps
  bin.size <- bin.length*frame.rate
  n.bins <- vid.length/bin.size
  ##############################


  ######fxn to melt WL data to long format#########
  WL.pos.long <- function(data, num.tracks) {
    subset.long <- data[,1:(num.tracks*2 + 2)] %>% melt(id.vars = c(1,2)) %>%
      separate(variable, sep = "\\.", c("worm", "pos")) %>% dcast(Frame + Time + worm ~ pos)
    return(subset.long)
  }

  ######fxn to calculate 3 point curvature based on law of cosines ####
  curve.angle <- function(del.x1, del.y1, del.x2, del.y2) {
    values <- list(del.x1, del.y1, del.x2, del.y2)
    if(anyNA(values)) {
      "NA"
    } else {
      x <- c(del.x1, del.y1)
      y <- c(del.x2, del.y2)
      dot.prod <- x%*%y
      norm.x <- as.numeric(svd(x)[1]) # faster to use svd and index (which for 1x1 vec is all norm does)
        #norm(x,type="2") # length of vector
      norm.y <- as.numeric(svd(y)[1])
        #norm(y,type="2")
      theta <- acos(dot.prod / (norm.x * norm.y))
      as.numeric(theta)
    }
  }
  #################################################


  ##### merge and get data ##########
  print(system.time(WL.centroid <- WL.pos.long(position, num.tracks) %>% mutate(type = "centroid")))

  WL.speed <- speed[,1:(num.tracks + 2)] %>%
    melt(id.vars = c(1,2)) %>%
    separate(variable, sep = "\\.", c("worm", "stuffer")) %>%
    rename(speed = value)

  WL.head.dir <- direction[,1:(num.tracks + 2)] %>%
    melt(id.vars = c(1,2)) %>%
    separate(variable, sep = "\\.", c("worm", "stuffer")) %>%
    rename(head.dir = value)

  WL.length <- length[,1:(num.tracks + 2)] %>%
    melt(id.vars = c(1,2)) %>%
    separate(variable, sep = "\\.", c("worm", "stuffer")) %>%
    rename(length = value)

  print(system.time(WL.alldata <- list(WL.centroid,WL.speed,WL.head.dir,WL.length)))
  #print(system.time(WL.alldata %<>% Reduce(function(...) merge(..., all = T), .)))
  print(system.time(WL.alldata %<>% Reduce(function(...) dplyr::full_join(...), .)))
  print(system.time(WL.alldata %<>% arrange(worm, Time)))
  print(system.time(WL.alldata %<>% mutate(stuffer = NULL))) ### this takes longest

  print(system.time(WL.alldata <- WL.alldata %>% group_by(worm) %>%
    mutate(del.y2 =  y - lag(y), # change from previous point (t-1) to (t0)
           del.x2 = x - lag(x),
           del.x1 = lag(x) - lag(x, n=2), #vector from t(-2) to t(-1) for curve angle
           del.y1 = lag(y) - lag(y, n=2),
           time.bin = ntile(Time, n.bins),
           curve.ang = as.numeric(mapply(curve.angle, del.x1, del.y1, del.x2, del.y2))*180/pi) %>% group_by(worm, time.bin) %>%
    mutate(bin.speed = abs(mean(speed, na.rm=TRUE)), bin.ang.vel = mean(curve.ang, na.rm=TRUE)) %>% filter(!is.na(curve.ang))))
  ###################################
  return(WL.alldata)
}

roam.pct <- function(data, slope) {
  roam <- WL.alldata %>% group_by(worm,time.bin) %>%
    summarize(mean.speed = mean(bin.speed),mean.angle = mean(bin.ang.vel)) %>%
    mutate(ratio = mean.speed/mean.angle) %>% dplyr::filter(ratio >= slope) %>% nrow()
  dwell <- WL.alldata %>% group_by(worm,time.bin) %>%
    summarize(mean.speed = mean(bin.speed),mean.angle = mean(bin.ang.vel)) %>%
    mutate(ratio = mean.speed/mean.angle) %>% dplyr::filter(ratio < slope) %>% nrow()
  pct.roam <- roam/dwell
  print(pct.roam)
  return(c(roam = roam, dwell = dwell, pct.roam = pct.roam))
}

########## get roaming data ############
system.time(WL.alldata<-WL.roam.data.edit(bin.length = 2, frame.rate = 6, num.tracks = 100)) ##### CHANGE # TRACKS!!!!!###

#plot density map of points:
WL.alldata %>% group_by(worm,time.bin) %>%
  dplyr::filter(bin.speed < 500) %>%
  summarize(mean.speed = mean(bin.speed),mean.angle = mean(bin.ang.vel)) %>%
  ggplot(aes(x = mean.angle, y = mean.speed)) +
  stat_density2d(geom="raster", aes(fill = ..density..), contour = FALSE)  +
  viridis::scale_fill_viridis(option = "inferno", begin = 0.05, end = 0.9) +
  coord_cartesian(xlim = c(0,100),ylim = c(0,250)) +
  geom_segment(aes(x=0, y=0, xend = 30, yend = 200), colour = "red") + theme_classic()

# get count roam v dwell:
pct.roam <- roam.pct(WL.alldata, slope = 200/30)
pct.roam


##### for visual inpection, plot all selected tracks ####
ggplotly(WL.alldata %>% dplyr::filter(Time < 1200) %>% ggplot(aes(x = x, y = y)) + geom_point(aes(colour = worm), alpha = 0.2) +
  scale_color_viridis(option = "inferno", discrete = TRUE) + facet_wrap(~ntile(Time,4))) #to plot each track

p<-WL.alldata %>% ggplot(aes(x=ntile(y, 800)))
gridExtra::grid.arrange(p+stat_summary(fun.y = "mean", geom = "point", aes(y=abs(speed))),
             p+stat_summary(fun.y = "mean", geom = "point", aes(y=abs(curve.ang))),
             p+stat_summary(fun.y = "mean", geom = "point", aes(y=bin.speed)),
             p+stat_summary(fun.y = "mean", geom = "point", aes(y=bin.ang.vel)))

ggplotly(WL.alldata %>% dplyr::filter(Time < 30, ang.vel < 100) %>% ggplot(aes(x = x, y = y)) + geom_point(aes(colour = Time), alpha = 0.5) +
               scale_color_viridis(option = "inferno")) #+ facet_wrap(~worm) #to plot each track


WL.alldata %>% ggplot(aes(x = x, y = y)) + geom_point(alpha = 0.2) +
  scale_color_viridis(option = "inferno", discrete = TRUE)

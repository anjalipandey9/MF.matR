


WL.pos.data <- function(bin.length, frame.rate, num.tracks) {
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(reshape2)
  library(viridis)
  #library(plotly)

  #### import data ### - use  readr to improve speed
  message("Choose position data")
  position <- read.csv(file.choose(), skip=4)
  # message("Choose Direction data")
  # direction <- read.csv(file.choose(), skip=4)
  # message("Choose speed data")
  # speed <- read.csv(file.choose(), skip=4)

  ####Setting up parameters####
  if(missing(num.tracks)) {
    num.tracks <- (length(position)/2) - 2
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

  # ######fxn to calculate 3 point curvature based on law of cosines ####
  # curve.angle <- function(del.x1, del.y1, del.x2, del.y2) {
  #   values <- list(del.x1, del.y1, del.x2, del.y2)
  #   if(anyNA(values)) {
  #     "NA"
  #   } else {
  #     x <- c(del.x1, del.y1)
  #     y <- c(del.x2, del.y2)
  #     dot.prod <- x%*%y
  #     norm.x <- as.numeric(svd(x)[1]) # faster to use svd and index (which for 1x1 vec is all norm does)
  #     #norm(x,type="2") # length of vector
  #     norm.y <- as.numeric(svd(y)[1])
  #     #norm(y,type="2")
  #     theta <- acos(dot.prod / (norm.x * norm.y))
  #     as.numeric(theta)
  #   }
  # }
  # #################################################


  ##### merge and get data ##########
  WL.centroid <- WL.pos.long(position, num.tracks = 10) %>% mutate(type = "centroid")

  #WL.speed <- speed[,1:(num.tracks + 2)] %>% melt(id.vars = c(1,2)) %>%  separate(variable, sep = "\\.", c("worm", "stuffer")) %>% rename(speed = value)

  #WL.head.dir <- direction[,1:(num.tracks + 2)] %>% melt(id.vars = c(1,2)) %>%  separate(variable, sep = "\\.", c("worm", "stuffer")) %>% rename(head.dir = value)

  WL.alldata <- list(WL.centroid) %>%
    Reduce(function(...) merge(..., all = T), .) %>% arrange(worm, Time) %>% mutate(stuffer = NULL)

  # WL.alldata <- WL.alldata %>% group_by(worm) %>%
  #   mutate(del.y2 =  y - lag(y), # change from previous point (t-1) to (t0)
  #          del.x2 = x - lag(x),
  #          del.x1 = lag(x) - lag(x, n=2), #vector from t(-2) to t(-1) for curve angle
  #          del.y1 = lag(y) - lag(y, n=2),
  #          # del.x2 = x - lag(x,n=2), #vector from t(-2) to t(0) for curve angle
  #          # del.y2 = y - lag(y,n=2),
  #          time.bin = ntile(Time, n.bins),
  #          curve.ang = as.numeric(mapply(curve.angle, del.x1, del.y1, del.x2, del.y2))*180/pi) %>% group_by(worm, time.bin) #%>%
    #mutate(bin.speed = mean(abs(speed), na.rm=TRUE), bin.ang.vel = mean(curve.ang, na.rm=TRUE)) %>% filter(!is.na(curve.ang))
  ###################################
  return(WL.alldata)
}

track.list <- WL.pos.data(bin.length = 2, frame.rate=6) # optional num.tracks = n for subset, bin.length in sec

track.list %>% ggplot() + geom_point(aes(x=x, y=y, colour = worm, alpha = .1)) +
  scale_color_viridis(option = "inferno", discrete = TRUE) #to plot each track

#' Import Wormlab data, merge and convert to long format with curve angle.
#' plus it assigns binary state columns to each behavioral state class -work in progress-
#' @param bin.length length of time bins in seconds. Used for state analysis
#' @param frame.rate video frame rate
#' @param num.tracks optional argument to limit input to certain number of worm tracks
#' @export
#' @examples
#' Import.WL.data(bin.length = 2, frame.rate = 6)

Import.WL.data <- function(bin.length, frame.rate, num.tracks) {

  #### import data ### - use  readr to improve speed
  message("Choose position data")
  print(system.time(position <- read.csv(file.choose(), skip=4)))
  message("Choose Direction data")
  direction <- read.csv(file.choose(), skip=4)
  message("Choose speed data")
  speed <- read.csv(file.choose(), skip=4)
  message("Choose length data")
  length <- read.csv(file.choose(), skip=4)
  message("Choose width data")
  width <- read.csv(file.choose(), skip=4)
  message("Choose Omega data")
  state <- read.csv(file.choose(), skip=4)


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

  WL.width <- width[,1:(num.tracks + 2)] %>%
    melt(id.vars = c(1,2)) %>%
    separate(variable, sep = "\\.", c("worm", "stuffer")) %>%
    rename(width = value)

  WL.state <- state[,1:(num.tracks + 2)] %>%
    melt(id.vars = c(1,2)) %>%
    separate(variable, sep = "\\.", c("worm", "stuffer")) %>%
    rename(state = value)

  print(system.time(WL.alldata <- list(WL.centroid,
                                       WL.speed,
                                       WL.head.dir,
                                       WL.length,
                                       WL.width,
                                       WL.state)))
  print(system.time(WL.alldata %<>% Reduce(function(...) dplyr::full_join(...), .)))
  print(system.time(WL.alldata %<>% arrange(worm, Time)))
  print(system.time(WL.alldata %<>% mutate(stuffer = NULL))) ### this takes longest

  print(system.time(WL.alldata <- WL.alldata %>% group_by(worm) %>%
                      mutate(del.y2 =  y - lag(y), # change from previous point (t-1) to (t0)
                             del.x2 = x - lag(x),
                             del.x1 = lag(x) - lag(x, n=2), #vector from t(-2) to t(-1) for curve angle
                             del.y1 = lag(y) - lag(y, n=2),
                             time.bin = ntile(Time, n.bins),
                             curve.ang = as.numeric(mapply(MF.matR::curve.angle, del.x1, del.y1, del.x2, del.y2))*180/pi) %>% group_by(worm, time.bin) %>%
                      mutate(bin.speed = abs(mean(speed, na.rm=TRUE)), bin.ang.vel = mean(curve.ang, na.rm=TRUE)) %>% filter(!is.na(curve.ang))))
  ###################################
  return(WL.alldata)
}

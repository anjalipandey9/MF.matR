#' Import Wormlab data, merge and convert to long format with curve angle.
#' plus it assigns binary state columns to each behavioral state class -work in progress-
#' @param bin.length length of time bins in seconds. Used for state analysis
#' @param frame.rate video frame rate
#' @param num.tracks optional argument to limit input to certain number of worm tracks
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
#' @examples
#' Import.WL.data(bin.length = 2, frame.rate = 6)

Import.WL.data <- function(bin.length = 4, frame.rate, num.tracks, multiple, head.correct = FALSE, ...) {
  library(tidyverse)

  #### import data ### - use  readr to improve speed
  if(missing(multiple)) {
    message("Choose a WormLab project file")
    filename <- file.choose()
  } else {
    filename <- mult_pos_file
  }
  #file.pref <- basename(filename) %>% strsplit(., c("Position","position")) %>% unlist()
  file.pref <- basename(filename) %>% strsplit(., c(".wdf", ".wpr")) %>% unlist()
  print(file.pref)
  folder <- dirname(filename)

  if(missing(multiple)) {
    file_list <- list.files(path = file.path(folder))
  } else {
    file_list  <- list.files(path = file.path(folder),pattern = file.pref[1])
  }


  position.path <- file.path(folder, file_list[grep("osition.csv",file_list)])
  direction.path <- file.path(folder, file_list[grep("irection.csv",file_list)])
  speed.path <- file.path(folder, file_list[grep("peed.csv",file_list)])
  length.path <- file.path(folder, file_list[grep("ength.csv",file_list)])
  width.path <- file.path(folder, file_list[grep("idth.csv",file_list)])
  omega.path <- file.path(folder, file_list[grep("mega",file_list)])


  message("loading in data")
  print(position.path)
  print(file_list)
  position <- read.csv(position.path, skip=4)
  message("position done")
  direction <- read.csv(direction.path, skip=4)
  message("direction done")
  speed <- read.csv(speed.path, skip=4)
  message("dir and speed done")
  length <- read.csv(length.path, skip=4)
  message("length done")
  width <- read.csv(width.path, skip=4)
  message("width done")
  state <- read.csv(omega.path, skip=4)

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
    subset.long <- data[,1:(num.tracks*2 + 2)] %>% reshape2::melt(id.vars = c(1,2)) %>%
      tidyr::separate(variable, sep = "\\.", c("worm", "pos")) %>% reshape2::dcast(Frame + Time + worm ~ pos)
    return(subset.long)
  }

  ##### merge and get data ##########
  message("merging datasets ...")
  print(system.time(WL.centroid <- WL.pos.long(position, num.tracks) %>% dplyr::mutate(type = "centroid")))

  WL.speed <- speed[,1:(num.tracks + 2)] %>%
    reshape2::melt(id.vars = c(1,2)) %>%
    tidyr::separate(variable, sep = "\\.", c("worm", "stuffer")) %>%
    dplyr::rename(speed = value)

  WL.move.dir <- direction[,1:(num.tracks + 2)] %>%
    reshape2::melt(id.vars = c(1,2)) %>%
    tidyr::separate(variable, sep = "\\.", c("worm", "stuffer")) %>%
    dplyr::rename(move.dir = value)

  WL.length <- length[,1:(num.tracks + 2)] %>%
    reshape2::melt(id.vars = c(1,2)) %>%
    tidyr::separate(variable, sep = "\\.", c("worm", "stuffer")) %>%
    dplyr::rename(length = value)

  WL.width <- width[,1:(num.tracks + 2)] %>%
    reshape2::melt(id.vars = c(1,2)) %>%
    tidyr::separate(variable, sep = "\\.", c("worm", "stuffer")) %>%
    dplyr::rename(width = value)

  WL.state <- state[,1:(num.tracks + 2)] %>%
    reshape2::melt(id.vars = c(1,2)) %>%
    tidyr::separate(variable, sep = "\\.", c("worm", "stuffer")) %>%
    dplyr::rename(state = value)

print(system.time(WL.alldata <- list(WL.centroid,
                                       WL.speed,
                                       WL.move.dir,
                                       WL.length,
                                       WL.width,
                                       WL.state)))
  print(system.time(WL.alldata <- WL.alldata %>% Reduce(function(...) dplyr::full_join(...), .)))
  print(system.time(WL.alldata <- WL.alldata %>% dplyr::arrange(worm, Time)))
  print(system.time(WL.alldata <- WL.alldata %>% dplyr::mutate(stuffer = NULL))) ### this takes longest

  message("calculating angular velocity (degrees/frame)")
  print(system.time(
    WL.alldata <- WL.alldata %>%
      dplyr::group_by(worm) %>%
      dplyr::mutate(del.y1 =  y - lag(y), # change from previous point (t-1) to (t0)
             del.x1 = x - lag(x),
             del.x2 = lag(x) - lag(x, n=2), #vector from t(-2) to t(-1) for curve angle
             del.y2 = lag(y) - lag(y, n=2),
             time.bin = cut(Time, n.bins),
             curve.ang = as.numeric(mapply(MF.matR::curve.angle, del.x1, del.y1, del.x2, del.y2))*180/pi) %>%
      dplyr::group_by(worm, time.bin) %>%
      dplyr::mutate(bin.speed = abs(mean(speed, na.rm=TRUE)),
             bin.ang.vel = mean(curve.ang, na.rm=TRUE)) %>%
      dplyr::filter(!is.na(curve.ang))
    ))

  if(head.correct == TRUE) {
    message("Correcting head direction")


    #setting run direction threshold


    WL.alldata <- WL.alldata %>%
      dplyr::mutate(run.dir = dplyr::case_when(speed < -30 ~ -1,
                                               speed > 30 ~ 1,
                                               TRUE ~ 0))

    # using rle(), get length of runs in each direction, then get max in REVERSE direction

    backwards <- purrr::possibly(WL.alldata %>%
                                   dplyr::group_by(worm) %>%
                                   dplyr::do({
                                     tmp <- cbind(with(rle(.$run.dir==-1), lengths[values]))
                                     data.frame(worm= .$worm, Max.back=if(length(tmp)==0) 0
                                                else max(tmp)) }) %>%
                                   dplyr::slice(1L))

    # using rle(), get length of runs in each direction, then get max in FORWARD direction
    forward <- purrr::possibly(WL.alldata %>%
                                 dplyr::group_by(worm) %>%
                                 dplyr::do({tmp <- cbind(with(rle(.$run.dir==1), lengths[values]))
                                 data.frame(worm= .$worm, Max.for=if(length(tmp)==0) 0
                                            else max(tmp)) }) %>%
                                 dplyr::slice(1L))

    max_runs <- dplyr::full_join(backwards,forward)

    # # switch direction of worms that have longer runs in reverse than forward
    WL.alldata <- WL.alldata %>%
      dplyr::mutate(
        speed = dplyr::case_when(
          worm %in% max_runs[max_runs$Max.back > max_runs$Max.for,]$worm ~ -(speed),
          TRUE ~ speed),
        corrected = dplyr::case_when(
          worm %in% max_runs[max_runs$Max.back > max_runs$Max.for,]$worm ~ 1,
          TRUE ~ 0),
        head.dir = dplyr::case_when(
          corrected == 0 &
            (move.dir %% 360) %% 360 < 155 &
            (move.dir %% 360) %% 360 > 25 ~ "up",
          corrected == 0 &
            (move.dir %% 360) %% 360 > 205 &
            (move.dir %% 360) %% 360 < 335 ~ "down",
          corrected == 0 &
            (move.dir %% 360) %% 360 > 155 &
            (move.dir %% 360) %% 360 < 205 ~ "right",
          corrected == 0 &
            ((move.dir %% 360) %% 360 < 25 |
               (move.dir %% 360) %% 360 > 335) ~ "left",
          corrected == 1 &
            (move.dir %% 360) %% 360 < 155 &
            (move.dir %% 360) %% 360 > 25 ~ "down",
          corrected == 1 &
            (move.dir %% 360) %% 360 > 205 &
            (move.dir %% 360) %% 360 < 335 ~ "up",
          corrected == 1 &
            (move.dir %% 360) %% 360 > 155 &
            (move.dir %% 360) %% 360 < 205 ~ "left",
          TRUE ~ "right"
        ))
  }

message("adding behavioral states")

print(system.time(WL.alldata <- WL.alldata %>%
  dplyr::mutate(state = dplyr::case_when(
    state == "Omega Bend" ~ "omega",
    abs(speed) < 30 ~ "pause",
    curve.ang > 150 ~ "reversal",
    speed < -30 ~ "backward",
    TRUE ~ "forward"
  ))))

data.table::fwrite(WL.alldata, file.path(folder,paste0(file.pref[1],"_all_track_data.csv")))

  return(WL.alldata)
}

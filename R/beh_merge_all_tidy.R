#' beh_merge_all_tidy function
#' combines functions beh_mat, y_mat, and dir_mat to
#' make a merged behavioral state object incorporating direction, position and state information
#' about each worm from Matlab tracking. Requires beh_mat, y_mat, and dir_mat to be defined by installing MF.matR package.
#' also required reshape2, tcl/tk and dplyr packages installed
#'
#' @param bins number of time frames to bin. generally 400 for 20 minute video is standard
#' @export
#' @examples tot.mat<-beh_merge_all_tidy(400)
#' beh_merge_all_tidy()

beh_merge_all_tidy <- function(interactive = FALSE) {
library(tidyverse)

  ##### file selection ~=##########
  message("select .mat file")
  folder <- dirname(file.choose())
  print(folder)
  ##################################


  message("reading behmat")

  ##### behmat ###############
  if(interactive == TRUE) {
    message("Choose behmat file")
    behmat<-readr::read_csv(file.choose(), col_names = FALSE)
    message("Choose ExpInfo file")
    ExpInfo <- readr::read_csv(file.choose(), col_names = TRUE)
  } else {
    behPath <- fs::dir_ls(folder, regex = "behmat")
    ExpInfoPath <- fs::dir_ls(folder, regex = "Expinfo")
    message("reading behmat file")
    behmat <- readr::read_csv(behPath, col_names = FALSE)
    ExpInfo <- readr::read_csv(ExpInfoPath, col_names = TRUE)
  }

  behmat<- ExpInfo %>%
    mutate(date = factor(date)) %>%
    expand_grid(., behmat) %>%
    mutate(worm = factor(seq(1:nrow(behmat))))
  behmat <- behmat %>% tidyr::gather(key = "time",
                                     value = "state",
                                     tidyselect::starts_with("X")) %>% #put all data columns (time = "t1 ... "t2400") into long format
    dplyr::mutate(time = as.numeric(gsub("[^0-9]", "", time)),
                  wormID = interaction(date, genotype, experimentNum, stimulus, condition, worm),
                  state_name = dplyr::case_when(
                    #!state %in% c(1:8) ~ "NA",
                    state == "1" ~ "Forward",
                    state == "2" ~ "Curve",
                    state == "3" ~ "Reverse",
                    state == "4" ~ "Pause",
                    state == "5" ~ "Omega-R",
                    state == "6" ~ "Omega-F",
                    state %in% c("7", "8") ~ "no_state_call",
                    TRUE ~ "NA"))
  #vid.time <- as.numeric(gsub("[^0-9]","", behmat$variable))

  message("reading ymat")
  ##### ymat ##########
  if(interactive == TRUE) {
    message("Choose ymat file")
    ymat<-read_csv(file.choose(), col_names = FALSE)
    message("choose preprocess file")
    preprocess <- readr::read_csv(file.choose(), col_names = TRUE) %>%
      select(pixelSize)
  } else {
    ymatPath <- fs::dir_ls(folder, regex = "ymat")
    preprocessPath <- fs::dir_ls(folder, regex = "preprocess.csv")
    ymat <- readr::read_csv(ymatPath, col_names = FALSE)
    pixelSize <- readr::read_csv(preprocessPath, col_names = TRUE) %>%
      select(pixelSize) %>% as.numeric()
  }
  ymat <- ymat %>%
    mutate(worm = factor(seq(1:nrow(ymat)))) %>%
    select(worm, everything()) %>%
    tidyr::gather(key = "time", value = "y", tidyselect::starts_with("X")) %>% #put all data columns (time = "t1 ... "t2400") into long format
    dplyr::mutate(time = as.numeric(gsub("[^0-9]","", time)),
                  y = round(-y/pixelSize, digits = 2))

  ##### xmat #############
  if(interactive == TRUE) {
    message("Choose xmat file")
    xmat<-read_csv(file.choose(), col_names = FALSE)
    message("choose preprocess file")
    preprocess <- readr::read_csv(file.choose(), col_names = TRUE) %>%
      select(pixelSize)
  } else {
    xmatPath <- fs::dir_ls(folder, regex = "xmat")
    preprocessPath <- fs::dir_ls(folder, regex = "preprocess.csv")
    xmat <- readr::read_csv(xmatPath, col_names = FALSE)
    pixelSize <- readr::read_csv(preprocessPath, col_names = TRUE) %>%
      select(pixelSize) %>% as.numeric()
  }
  xmat <- xmat %>%
    mutate(worm = factor(seq(1:nrow(xmat)))) %>%
    select(worm, everything()) %>%
    tidyr::gather(key = "time", value = "x", tidyselect::starts_with("X")) %>% #put all data columns (time = "t1 ... "t2400") into long format
    dplyr::mutate(time = as.numeric(gsub("[^0-9]","", time)),
                  x = round(-x/pixelSize, digits = 2))
  message("reading dirmat")
  ##### dirmat ##########
  if(interactive == TRUE) {
    message("Choose dirmat file")
    dirmat<-read_csv(file.choose(), col_names = FALSE)
  } else {
    dirmatPath <- fs::dir_ls(folder, regex = "dirmat")
    dirmat <- readr::read_csv(dirmatPath, col_names = FALSE)
  }

  dirmat <- dirmat %>%
    mutate(worm = factor(seq(1:nrow(dirmat)))) %>%
    select(worm, everything()) %>%
    tidyr::gather(key = "time", value = "dir", tidyselect::starts_with("X")) %>% #put all data columns (time = "t1 ... "t2400") into long format
    dplyr::mutate(time = as.numeric(gsub("[^0-9]","", time)),
                  dir.bin = cut(dir, c(0,45,135,225,315,360), dig.lab=10, na.rm = TRUE),
                  direction = dplyr::case_when(
                    dir.bin %in% c("(0,45]", "(315,360]") ~ "right",
                    dir.bin == "(45,135]" ~ "up",
                    dir.bin == "(135,225]" ~ "left",
                    dir.bin == "(225,315]" ~ "down"
                  ))
  ##### file merge #########
  tot.mat<-dplyr::left_join(behmat, ymat) %>%
    left_join(., xmat) %>%
    left_join(., dirmat)
  ##### write data
  message("writing trackdata to disk")
  readr::write_csv(tot.mat, file.path(folder,"all_matTrack_data.csv"))
  return(tot.mat)
  # return(list(behmat, ymat, dirmat))
}


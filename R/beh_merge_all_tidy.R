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

beh_merge_all_tidy <- function() {
  message("select .mat file")
  folder <- dirname(file.choose())
  message("reading behmat")
  behmat<-MF.matR::beh_mat_tidy(interactive = FALSE)
  #vid.time <- as.numeric(gsub("[^0-9]","", behmat$variable))
  message("reading ymat")
  ymat<-MF.matR::y_mat_tidy(interactive = FALSE)
  message("reading xmat")
  xmat<-MF.matR::x_mat_tidy(interactive = FALSE)
  message("reading dirmat")
  dirmat<-MF.matR::dir_mat_tidy(interactive = FALSE)
  message("reading spdmat")
  spdmat<-MF.matR::spd_mat_tidy(interactive = FALSE)
  tot.mat<-dplyr::left_join(behmat, ymat) %>%
    left_join(., xmat) %>%
    left_join(., dirmat) %>%
    left_join(., spdmat)
  readr::write_csv(tot.mat, file.path(folder,"all_matTrack_data.csv"))
  return(tot.mat)
  # return(list(behmat, ymat, dirmat))
}

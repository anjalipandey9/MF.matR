#' beh_merge_all function
#' combines functions beh_mat, y_mat, and dir_mat to
#' make a merged behavioral state object incorporating direction, position and state information
#' about each worm from Matlab tracking. Requires beh_mat, y_mat, and dir_mat to be defined by installing MF.matR package.
#' also required reshape2, tcl/tk and dplyr packages installed
#'
#' @param bins number of time frames to bin. generally 400 for 20 minute video is standard
#' @export
#' @examples tot.mat<-beh_merge_all(400)
#' beh_merge_all()

beh_merge_all <- function(bins) {
  behmat<-MF.matR::beh_mat(bins)
  vid.time <- as.numeric(gsub("[^0-9]","", behmat$variable))
  ymat<-MF.matR::y_mat(bins, time)
  dirmat<-MF.matR::dir_mat(bins)
  tot.mat<-left_join(behmat, ymat)
  tot.mat<-left_join(tot.mat, dirmat)
  return<-tot.mat
}

#' beh_merge_all function
#' combines functions beh_mat, y_mat, and dir_mat to
#' make a merged behavioral state object incorporating direction, position and state information
#' about each worm from Matlab tracking. Requires beh_mat, y_mat, and dir_mat to be defined by installing MF.matR package.
#' also required reshape2, tcl/tk and dplyr packages installed
#'
#' @param n number of time frames to bin. generally 400 for 20 minute video is standard
#' @export
#' @examples tot.mat<-beh_merge_all(400)
#' beh_merge_all()

beh_merge_all <- function(n) {
  behmat<-beh_mat(n)
  ymat<-y_mat(n)
  dirmat<-dir_mat(n)
  tot.mat<-left_join(behmat, ymat)
  tot.mat<-left_join(tot.mat, dirmat)
  return<-tot.mat
}

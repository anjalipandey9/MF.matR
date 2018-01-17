#' lingrad_mirror function
#' takes linear gradient data and mirrors for data with gradient in opposite orientation
#' For experiments with top of arena is max(cue) then orient == 1, else orient == 0 to switch
#'
#' @param df is the input data frame > should be output from beh_merge_all ie df
#' @export
#' @examples
#' tot.mat<-lingrad_mirror(df)

lingrad_mirror <- function (df) {
  df<-beh_merge_all(400)
  df<-df[order(df[,'wormID'], df[,'time']), ] # order by wormID, then time
  #df$orient = 1
  from<-c("up", "down")
  to<-c("down", "up")
  df$direction <- ifelse(df$orient == 0, as.character(plyr::mapvalues(df$direction, from = from, to = to)), as.character(df$direction))
  from <- c("left", "right")
  to <- c("side", "side")
  df$direction <-  as.character(plyr::mapvalues(df$direction, from = from, to = to))
  df$pos <- ifelse(df$orient == 0, abs(df$pos) - 16, df$pos)
  return(df)
}

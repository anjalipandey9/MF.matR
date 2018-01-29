#' norm_beh_exit function
#' Normalizes behavioral data +/- 2.5mm for animals EXITING a
#' single CENTER stripe of a chemotaxis cue.
#' @param df input merged data.frame containing output from beh_mat, dir_mat and y_mat files.
#' @param bounds optional argument to manually specify cue position boundaries. Should be manually entered for
#' matlab data for now
#' @export
#' @examples
#' norm_beh_exit()

norm_beh_exit<-function(df,bounds) {
  if(missing(bounds)) {
    message("no 'bounds' argument, will calculate automatically, assuming stripe is centered and takes half arena")
    bounds <- quantile(c(min(df$y),max(df$y)))
  }
  # for Matlab data, in which units are in mm, and y positions are negative (top = 0):
  if(min(bounds) < 0) {
  #worms moving down from the middle of image
  df.down<-df[df[,'head.dir'] == "down" & (df[,'y'] < (bounds[3]+2.5) & df[,'y'] > (bounds[3]-2.5)),]
  df.down$norm.y<-df.down[,'y']-bounds[3]
  df.down$norm.y<- (-df.down[,'norm.y'])

  #worms moving up from the middle
  df.up<-df[df[,'head.dir'] == "up" & (df[,'y'] < (bounds[2]+2.5) & df[,'y'] > (bounds[2]-2.5)),]
  df.up$norm.y<-df.up[,'y']-bounds[2]
  tot.mat.norm.1<-merge(df.down, df.up, all=TRUE)
  tot.mat.norm.1$norm.y<-round((tot.mat.norm.1$norm.y), digits = 1)
  return(tot.mat.norm.1)
  } else {
    # for Wormlab data, in which units are microns and y position is postitive and relative to video pixels
    # example bounds = c(1000,2250,3500,4750,6000) = 0%,25%,50%,75%,100%iles
    #worms moving down from the middle of image
    df.down <-  df %>% dplyr::filter(head.dir == "down" & y < bounds[2] + 1000 & y > bounds[2] - 1000) %>%
      dplyr::mutate(norm.y = - (y - bounds[2])) #bring center to 0 and invert to get upper animals (prior to exit) as negative
    #worms moving up from the bottom
    df.up   <-  df %>% dplyr::filter(head.dir == "up" & y > bounds[4] - 1000 & y < bounds[4] + 1000) %>%
      dplyr::mutate(norm.y = y - bounds[4])
    tot.mat.norm<-rbind(df.down, df.up)
    return(tot.mat.norm)
  }
}

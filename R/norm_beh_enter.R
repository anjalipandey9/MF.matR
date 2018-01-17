#' norm_beh_enter function
#' Normalizes behavioral data +/- 2.5mm for animals entering a
#' single stripe of chemotaxis cue.
#' @param df input merged data.frame containing output from beh_mat, dir_mat and y_mat files.
#' @export
#' @examples
#' norm_beh_enter()

norm_beh_enter<-function(df) {
  df.down<-df[df[,'head.dir'] == "down" & (df[,'pos'] < (bounds[2]+2.5) & df[,'pos'] > (bounds[2]-2.5)),]
  df.down$norm.pos<-df.down[,'pos']-bounds[2]
  df.down$norm.pos<- (-df.down[,'norm.pos'])

  df.up<-df[df[,'head.dir'] == "up" & (df[,'pos'] < (bounds[3]+2.5) & df[,'pos'] > (bounds[3]-2.5)),]
  df.up$norm.pos<-df.up[,'pos']-bounds[3]
  tot.mat.norm<-merge(df.down, df.up)
  tot.mat.norm$norm.pos<-round((tot.mat.norm$norm.pos), digits = 1)
  return(tot.mat.norm)
}

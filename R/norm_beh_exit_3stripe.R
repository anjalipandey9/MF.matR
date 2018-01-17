#' norm_beh_exit_3 function
#' Normalizes behavioral data +/- 2.5mm for animals EXITING
#' 3 stripes of chemotaxis cue.
#' @param df input merged data.frame containing output from beh_mat, dir_mat and y_mat files.
#' @export
#' @examples
#' norm.exit<-norm_beh_exit_3(df)

norm_beh_exit_3<-function(df) {
  df.down1<-df[df[,'head.dir'] == "down" & (df[,'pos'] < (bounds[2]+2.5) & df[,'pos'] > (bounds[2]-2.5)),]
  df.down1$norm.pos<-df.down1[,'pos']-bounds[2]

  df.down2<-df[df[,'head.dir'] == "down" & (df[,'pos'] < (bounds[4]+2.5) & df[,'pos'] > (bounds[4]-2.5)),]
  df.down2$norm.pos<-df.down2[,'pos']-bounds[4]

  df.down<-merge(df.down1, df.down2, all=TRUE) # join 'down' subsets
  df.down$norm.pos<-(-df.down[,'norm.pos'])

  df.up1<-df[df[,'head.dir'] == "up" & (df[,'pos'] < (bounds[3]+2.5) & df[,'pos'] > (bounds[3]-2.5)),]
  df.up1$norm.pos<-df.up1[,'pos']-bounds[3]

  df.up2<-df[df[,'head.dir'] == "up" & (df[,'pos'] < (bounds[5]+2.5) & df[,'pos'] > (bounds[5]-2.5)),]
  df.up2$norm.pos<-df.up2[,'pos']-bounds[5]

  df.up<-merge(df.up1, df.up2, all=TRUE) # join 'up' subsets

  tot.mat.norm<-merge(df.down, df.up, all=TRUE)
  tot.mat.norm$norm.pos<-round((tot.mat.norm$norm.pos), digits = 1)
  return(tot.mat.norm)
}

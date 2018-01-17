#' fix_reversals function
#' attempts to fix mis-ID'ed reversals and carry forward any directional information.
#' For example, it uses previous direction to assign this value for pirouettes and pauses
#' requires plyr, zoo, dplyr and some data.table
#'
#' @param df is the input data frame > should be output from beh_merge_all() ie df or lingrad_mirror()
#' @export
#' @examples
#' tot.mat<-fix_reversals(df)


fix_reversals <- function (df) {
  df[,c("dir", "dir.bin", "direction")]<-zoo::na.locf(df[,c("dir", "dir.bin", "direction")])
  #df<-zoo::na.locf
  # carried forward all non NA values into NA fields. Most(all) are pause. Assumes Pause has same angle of previous state. Need to fix
  # fix for post-reversal pause angles, also need fix for broken tracks.
  df$dir<-as.numeric(df$dir)

  #fix mis-annotated curves that are really reversals (some of these are missed pirouettes):
  df$Rev <- ifelse(df$Curve == 1 &
                     abs(c(NA, diff(df$dir, lag=1))) > 120 &
                     c(NA,diff(df$Curve, lag = 1) == 1), # fix too large angle for Curves following For but need to avoid changing postRev
                   1, ifelse(df$Curve == 1 &
                               data.table::shift(df$Curve, 1L, type="lag") == 0 &
                               data.table::shift(df$Rev, 1L, type="lag") == 1 &
                               abs(c(NA, diff(df$dir, lag=1))) < 45, # fix when Curve is really last step in reversal
                             1, df$Rev))
  df$Curve <- ifelse(df$Rev == 1, 0, df$Curve) # if converted to reversal, set Curve to 0

  df<-mutate(df, del.ang.temp = dir - lag(dir)) # find change in angle to from previous
  df$del.ang.temp<-abs(df$del.ang.temp) # take abs value for negatives

  # fix head angle for reversals (most cases it's about 180º from previous heading, unless it's a string of reversals)
  df$head.ang<-ifelse(df$Rev == 1 & abs(c(NA, diff(df$dir, lag=1))) > 120, # if reversals accompany large dir change
                      df$dir - 180,
                      ifelse(df$Rev == 1 & c(NA,diff(df$Rev, lag = 1) == 0),
                             df$dir - 180,
                             ifelse(df$Rev == 1 &
                                      data.table::shift(df$Pau, 1L, type="lag") == 1 &
                                      abs(c(NA, diff(df$dir, lag=1))) > 120, # if you have a run of reversals, then apply as above
                                    df$dir - 180, df$dir))) # if reversing, keep angle same by subtracting 180 - need to fix diff neg values

  df$head.ang<-ifelse(df$omR == 1 & abs(c(NA, diff(df$dir, lag=1))) > 120, # if Pir-reverse accompany large dir change
                      df$dir - 180,
                      ifelse(df$omR == 1 & c(NA,diff(df$Rev, lag = 1) == 0), # if you have a run of Pir reversals, then apply as above
                             df$dir - 180, df$head.ang)) # gives some NAs


  df$head.ang<-ifelse(df$head.ang<0, df$head.ang+360, df$head.ang) # if resulting angle <0 add 360 to make +

  from<-c("up", "down") # convert to opposite direction for reversing animals
  to<-c("down", "up")
  df$head.dir<-ifelse(df$Rev == 1 & abs(c(NA, diff(df$dir, lag=1))) > 120,
                      as.character(plyr::mapvalues(df$direction, from = from, to = to)),
                      ifelse(df$Rev == 1 & c(NA,diff(df$Rev, lag = 1) == 0),
                             as.character(plyr::mapvalues(df$direction, from = from, to = to)),
                             as.character(df$direction))) # this switches the reversal direction factor if animal is reversing

  #set pirouettes to previous direction:
  df$head.dir<-ifelse(df$Pir == 1,NA,df$head.dir) #first, make all pirouette head.dir = NA
  df$head.dir[1] = df$direction[1]
  df$head.dir<-as.factor(zoo::na.locf(df$head.dir)) # then use na.locf to carry forward previous direction

  # make del.ang.temp NULL here

  # determine angle change to next time point,  all beh states - need to fix transition between wormID
  df<-mutate(df, del.ang= head.ang - lead(head.ang)) # find change in angle to next - !!if there's a pause this results in 180 output!!
  df$del.ang<-abs(df$del.ang)
  df$del.ang.abs<-ifelse(df$del.ang>180, 360-df$del.ang, df$del.ang) # make the angle less than 180º
  df$post.rev<-ifelse(df$Rev == 1 &
                        c(diff(df$Rev, lead = 1),NA == 1), df$del.ang.abs, NA) # this still needs to be fixed, pause is biggest prob
  return(df)
}


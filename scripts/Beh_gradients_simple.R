# load in required packages, if not already done
library(reshape2)
library(ggplot2)
library(stats)
library(dplyr)
library(boot)
library(splines)
library(scales)
library(tcltk)
library(devtools)
install('/Users/mikeod/GitHub/wormTracker/R_packages/MF.matR')
library(MF.matR)

# make 3 objects using above functions in long format and merge set time
# set time bin in function by number of frames - still need to fix/standardize ypos
tot.mat<-beh_merge_all(400)
tot.mat<-tot.mat[order(tot.mat[,'wormID'], tot.mat[,'time']), ] # order by wormID, then time


# make this a fix_reversals function:
tot.mat[,c("dir", "dir.bin", "direction")]<-zoo::na.locf(tot.mat[,c("dir", "dir.bin", "direction")])
# carried forward all non NA values into NA fields. Most(all) are pause. Assumes Pause has same angle of previous state. Need to fix
# fix for post-reversal pause angles.
tot.mat$dir<-as.numeric(tot.mat$dir)

#fix mis-annotated curves that are really reversals (some of these are missed pirouettes):
tot.mat$Rev <- ifelse(tot.mat$Curve == 1 &
                        abs(c(NA, diff(tot.mat$dir, lag=1))) > 120 &
                        c(NA,diff(tot.mat$Curve, lag = 1) == 1), # fix too large angle for Curves following For but need to avoid changing postRev
                                      1, ifelse(tot.mat$Curve == 1 &
                                                  shift(tot.mat$Curve, 1L, type="lag") == 0 &
                                                  shift(tot.mat$Rev, 1L, type="lag") == 1 &
                                                  abs(c(NA, diff(tot.mat$dir, lag=1))) < 45, # fix when Curve is really last step in reversal
                      1, tot.mat$Rev))
tot.mat$Curve <- ifelse(tot.mat$Rev == 1, 0, tot.mat$Curve) # if converted to reversal, set Curve to 0

tot.mat<-mutate(tot.mat, del.ang.temp = dir - lag(dir)) # find change in angle to from previous
tot.mat$del.ang.temp<-abs(tot.mat$del.ang.temp) # take abs value for negatives

# fix head angle for reversals (most cases it's about 180º from previous heading, unless it's a string of reversals)
tot.mat$head.ang<-ifelse(tot.mat$Rev == 1 & abs(c(NA, diff(tot.mat$dir, lag=1))) > 120, # if reversals accompany large dir change
                         tot.mat$dir - 180,
                         ifelse(tot.mat$Rev == 1 & c(NA,diff(tot.mat$Rev, lag = 1) == 0),
                                tot.mat$dir - 180,
                                ifelse(tot.mat$Rev == 1 &
                                         shift(tot.mat$Pau, 1L, type="lag") == 1 &
                                         abs(c(NA, diff(tot.mat$dir, lag=1))) > 120, # if you have a run of reversals, then apply as above
                                tot.mat$dir - 180, tot.mat$dir))) # if reversing, keep angle same by subtracting 180 - need to fix diff neg values

tot.mat$head.ang<-ifelse(tot.mat$omR == 1 & abs(c(NA, diff(tot.mat$dir, lag=1))) > 120, # if Pir-reverse accompany large dir change
                         tot.mat$dir - 180,
                         ifelse(tot.mat$omR == 1 & c(NA,diff(tot.mat$Rev, lag = 1) == 0), # if you have a run of Pir reversals, then apply as above
                                tot.mat$dir - 180, tot.mat$head.ang)) # gives some NAs


tot.mat$head.ang<-ifelse(tot.mat$head.ang<0, tot.mat$head.ang+360, tot.mat$head.ang) # if resulting angle <0 add 360 to make +

from<-c("up", "down", "left", "right") # convert to opposite direction for reversing animals
to<-c("down", "up", "right", "left")
tot.mat$head.dir<-ifelse(tot.mat$Rev == 1 & abs(c(NA, diff(tot.mat$dir, lag=1))) > 120,
                         as.character(plyr::mapvalues(tot.mat$direction, from = from, to = to)),
                         ifelse(tot.mat$Rev == 1 & c(NA,diff(tot.mat$Rev, lag = 1) == 0),
                                as.character(plyr::mapvalues(tot.mat$direction, from = from, to = to)),
                         as.character(tot.mat$direction))) # this switches the reversal direction factor if animal is reversing

#set pirouettes to previous direction:
tot.mat$head.dir<-ifelse(tot.mat$Pir == 1,NA,tot.mat$head.dir) #first, make all pirouette head.dir = NA
tot.mat$head.dir<-as.factor(zoo::na.locf(tot.mat$head.dir)) # then use na.locf to carry forward previous direction

# make del.ang.temp NULL here

# determine angle change to next time point,  all beh states - need to fix transition between wormID
tot.mat<-mutate(tot.mat, del.ang= head.ang - lead(head.ang)) # find change in angle to next - !!if there's a pause this results in 180 output!!
tot.mat$del.ang<-abs(tot.mat$del.ang)
tot.mat$del.ang.abs<-ifelse(tot.mat$del.ang>180, 360-tot.mat$del.ang, tot.mat$del.ang) # make the angle less than 180º
tot.mat$post.rev<-ifelse(tot.mat$Rev == 1 &
                           c(diff(tot.mat$Rev, lead = 1),NA == 1), tot.mat$del.ang.abs, NA) # this still needs to be fixed, pause is biggest prob



timbin_beh<-aggregate(cbind(Pir, For, Rev, Curve, Pau) ~ condition + pos + bin + head.dir, data = tot.mat, FUN=mean)

del.ang<-aggregate(del.ang.abs ~ condition + pos + bin + head.dir, data=subset(tot.mat, tot.mat$Rev==1), FUN = median) # took out wormID

# all states
plot<-ggplot(timbin_beh, aes(x=pos)) +
  geom_smooth(aes(y=Pir, linetype=condition), method=loess, level=0.99, alpha=0.15, colour="blue") +
  geom_smooth(aes(y=Rev, linetype=condition), method=loess, level=0.99, alpha=0.15,colour="red") +
  geom_smooth(aes(y=Curve, linetype=condition), method=loess, level=0.99, alpha=0.15,colour="orange") +
  geom_smooth(aes(y=For, linetype=condition), method=loess, level=0.99, alpha=0.15,colour="green") +
  geom_smooth(aes(y=Pau, linetype=condition), method=loess, level=0.99, alpha=0.15,colour="yellow") +
  coord_cartesian(ylim=c(0, 1)) +
  facet_grid(.~head.dir) +
  theme_my
plot

# plot change in angle at subsequent time point - but for long revs or runs this will reduce mean angle.
plot1<-ggplot(del.ang, aes(x=pos, y=del.ang.abs, colour=condition)) +
                geom_jitter(alpha=0.1) +
                geom_smooth(aes(linetype=condition), method = loess, level=0.95, alpha=0.15, colour="blue") +
                facet_grid(.~head.dir) +
                coord_cartesian(ylim = c(0,75)) +
                theme_my
plot1


plot2 <-ggplot(tot.mat, aes(x=pos, y=post.rev)) +
  geom_jitter(alpha=0.1) +
  geom_smooth(aes(linetype=condition), method = loess, level=0.95, alpha=0.15, colour="blue") +
  facet_grid(.~head.dir) +
  scale_y_continuous(limits=c(0,75))+
  #coord_cartesian(ylim = c(0,150)) +
  theme_my
plot2

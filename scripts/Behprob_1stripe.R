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
#install('~/MF.matR')

# make 3 objects using above functions in long format
behmat<-beh_mat(n) # make DF with time bin of 240 frames = 120 sec
ymat<-y_mat(n)  # ditto for position data
dirmat<-dir_mat(n)

# merge the 3 datasets
tot.mat<-left_join(behmat, ymat)
tot.mat<-left_join(tot.mat, dirmat)

bounds.1<-c(0, -5, -11, -16) #set bounds for dye
tot.mat.norm.enter<-norm_beh_enter(tot.mat)
norm_timbin_beh_enter<-aggregate(cbind(Pir, For, Rev, Curve) ~ genotype + norm.pos + bin + condition + wormID, data = tot.mat.norm.enter, FUN=mean)
tot.mat.norm.exit<-norm_beh_exit(tot.mat)
norm_timbin_beh_exit<-aggregate(cbind(Pir, For, Rev, Curve) ~ genotype + norm.pos + bin + condition + wormID, data = tot.mat.norm.exit, FUN=mean)


# plot animals entering center stripe
plot1<-ggplot(norm_timbin_beh_enter, aes(x=norm.pos)) +
  annotate("rect", 0,0,-2.5,-0,0,1, fill="lightblue", alpha=0.3) +
  geom_smooth(aes(y=Pir, linetype=condition), level=0.99, alpha=0.15, colour="blue") +
  geom_smooth(aes(y=Rev, linetype=condition), level=0.99, alpha=0.15,colour="red") +
  geom_smooth(aes(y=Curve, linetype=condition), level=0.99, alpha=0.15,colour="orange") + 
  geom_smooth(aes(y=For, linetype=condition), level=0.99, alpha=0.15,colour="green") +
  coord_cartesian(ylim=c(0, 1)) + 
  facet_grid(genotype~.) +
  theme_my
plot1

# plot animals exiting center stripe
plot2<-ggplot(norm_timbin_beh_exit, aes(x=norm.pos)) +
  annotate("rect", 0,0,-2.5,-0,0,1, fill="lightblue", alpha=0.3) +
  geom_smooth(aes(y=Pir, linetype=condition), level=0.99, alpha=0.15, colour="blue") +
  geom_smooth(aes(y=Rev, linetype=condition), level=0.99, alpha=0.15,colour="red") +
  geom_smooth(aes(y=Curve, linetype=condition), level=0.99, alpha=0.15,colour="orange") + 
  geom_smooth(aes(y=For, linetype=condition), level=0.99, alpha=0.15,colour="green") +
  coord_cartesian(ylim=c(0, 1)) + 
  facet_grid(genotype~.) +
  theme_my
plot2

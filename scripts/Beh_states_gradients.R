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

# make 3 objects using above functions in long format
behmat<-beh.mat(n) # make DF with time bin of 240 frames = 120 sec
ymat<-y.mat(n)  # ditto for position data
dirmat<-dir.mat(n)

# merge the 3 datasets
tot.mat<-left_join(behmat, ymat)
tot.mat<-left_join(tot.mat, dirmat)

timbin_beh<-aggregate(cbind(Pir, For, Rev, Curve) ~ genotype + pos + bin + condition + wormID, data = tot.mat, FUN=mean)

plot<-ggplot(norm_timbin_beh, aes(x=pos)) +
  geom_smooth(aes(y=Pir, linetype=condition), method = loess, level=0.99, alpha=0.15, colour="blue") +
  geom_smooth(aes(y=Rev, linetype=condition), method = loess, level=0.99, alpha=0.15,colour="red") +
  geom_smooth(aes(y=Curve, linetype=condition), method = loess, level=0.99, alpha=0.15,colour="orange") + 
  geom_smooth(aes(y=For, linetype=condition), method = loess, level=0.99, alpha=0.15,colour="green") +
  coord_cartesian(ylim=c(0, 1)) + 
  facet_grid(genotype~.) +
  theme_my
plot
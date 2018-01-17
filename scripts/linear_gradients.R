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
tot.mat<-lingrad_mirror(tot.mat) # load in and mirror data if necessary * see lingrad_mirror docs
tot.mat<-fix_reversals(tot.mat)

timbin_beh<-aggregate(cbind(Pir, For, Rev, Curve, Pau) ~ condition + pos + bin + head.dir, data = tot.mat, FUN=mean)

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


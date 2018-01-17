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
install('C:/Users/Anna/Dropbox/matlab/wormTracker/R_packages/MF.matR')
library(MF.matR)

# make 3 objects using above functions in long format and merge set time
# set time bin in function by number of frames - still need to fix/standardize ypos
tot.mat<-lingrad_mirror(tot.mat) # load in and mirror data if necessary * see lingrad_mirror docs
tot.mat<-fix_reversals(tot.mat)


timbin_beh<-aggregate(cbind(Pir, For, Rev, Curve, Pau) ~ condition + pos + bin + head.dir, data = tot.mat, FUN=mean)

theme_my <- theme_bw() + theme(
  axis.line        = element_line(colour = "black"),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.border     = element_blank(),
  strip.background = element_blank(),
  legend.key       = element_blank(), 
  axis.text.x=element_text(angle=45, hjust=1, size=12)
)

# all states
plot<-ggplot(timbin_beh, aes(x=pos)) +
  #geom_jitter(alpha=0.2, aes(y=Pir)) +
  geom_smooth(aes(y=Pir, linetype=condition), method=loess, level=0.99, alpha=0.15, colour="blue", lwd=2) +
  geom_smooth(aes(y=Rev, linetype=condition), method=loess, level=0.99, alpha=0.15,colour="red", lwd=2) +
  geom_smooth(aes(y=Curve, linetype=condition), method=loess, level=0.99, alpha=0.15,colour="orange", lwd=2) +
  geom_smooth(aes(y=For, linetype=condition), method=loess, level=0.99, alpha=0.15,colour="green", lwd=2) +
  geom_smooth(aes(y=Pau, linetype=condition), method=loess, level=0.99, alpha=0.15,colour="yellow", lwd=2) +
  coord_cartesian(ylim=c(0, 0.6)) +
  facet_grid(.~head.dir) +
  theme_my
plot


library(reshape2)
library(ggplot2)
# need a 20 min merged csv with "condition", "exp", "exp", "date","stimulus"
# can add others if need be - all these could be made into one fucntion
read.binsize <- function() { 
  n <- readline(prompt="Enter number of time bins: ")
  if(!grepl("^[0-9]+$",n))
    {
    return(read.binsize())
    }
  return(as.integer(n))
}
n<-read.binsize()
print(readinteger())
# set up matlab multi-experiment .csv MF data with this function
for.mat<- function (n) {
  environment<-globalenv()
  behmat<-read.csv(file.choose())
  id.cols<-c(colnames(behmat[1:(length(behmat)-2400)])) # use all but state columns as IDs
  behmat[, id.cols] <- lapply(behmat[,id.cols], factor) # convert all ID columns to factors
  behmat<-melt(behmat, id.vars=id.cols) # convert to long
  colnames(behmat)[(length(behmat)-1):length(behmat)]<-c("variable", "state") # make last column "state"
  behmat$time<-as.numeric(gsub("X", "", behmat$variable)) # add time column, drop X resulting from melt
  behmat$variable<-NULL
  behmat<-subset(behmat, behmat$state<7) # eliminate untracked #7/8
  behmat$wormID<-behmat$exp:behmat$condition:behmat$animal # need to generalize this
  behmat$bin<- cut(behmat$time, seq(0,max(behmat$time), by = n), dig.lab=10)
  behmat$omF[behmat$state == 5] <- 1 ## omega forward = 1 for glm
  behmat$omF[is.na(behmat['omF'])]<-0 ## all else = 0
  behmat$omR[behmat$state == 6] <- 1 ## omega reverse = 1 for glm
  behmat$omR[is.na(behmat['omR'])]<-0 ## all else = 0
  behmat$For[behmat$state == 1] <- 1 ## forward = 1 for glm
  behmat$For[is.na(behmat['For'])]<-0 ## all else = 0
  behmat$Curve[behmat$state == 2] <- 1 ## forward = 1 for glm
  behmat$Curve[is.na(behmat['Curve'])]<-0 ## all else = 0
  behmat$Pau[behmat$state == 4] <- 1 ## forward = 1 for glm
  behmat$Pau[is.na(behmat['Pau'])]<-0 ## all else = 0
  behmat$Rev[behmat$state == 3] <- 1 ## forward = 1 for glm
  behmat$Rev[is.na(behmat['Rev'])]<-0 ## all else = 0
  behmat$Pir[behmat$state > 4 & behmat$state <  7] <- 1 ## all pirouettes = 1
  behmat$Pir[is.na(behmat['Pir'])]<-0 ## all else = 0
  behmat$state<-as.factor(behmat$state)
  return(data.frame(behmat))
}

# function assigns x position to each worm - need to register relative to stimulus position
y.mat <- function (n) {
  environment<-globalenv()
  ymat<-read.csv(file.choose())
  id.cols<-c(colnames(ymat[1:(length(ymat)-2400)])) # use all but state columns as IDs
  ymat[, id.cols] <- lapply(ymat[,id.cols], factor) # convert all ID columns to factors
  ymat<-melt(ymat, id.vars=id.cols) # convert to long
  colnames(ymat)[(length(ymat)-1):length(ymat)]<-c("variable", "pos") # make last column "pos"
  ymat$pos<-round(-(ymat$pos)/(650/16), digits = 1) # convert 650 pixels? to 16 mm
  ymat$time<-as.numeric(gsub("X", "", ymat$variable)) # add time column, drop X resulting from melt
  ymat$variable<-NULL
  ymat$wormID<-ymat$exp:ymat$condition:ymat$animal # need to generalize this
  #ymat$bin<- cut(ymat$time, seq(0,max(ymat$time), by = n), dig.lab=10)
  ymat$pos.bin<-cut(ymat$pos, 30, dig.lab=10)
  return(data.frame(ymat))
}

dir.mat <- function (n) {
  environment<-globalenv()
  dirmat<-read.csv(file.choose())
  id.cols<-c(colnames(dirmat[1:(length(dirmat)-2400)])) # identify all non time columns
  dirmat[, id.cols] <- lapply(dirmat[,id.cols], factor) # convert all ID columns to factors
  dirmat<-melt(dirmat, id.vars=id.cols) # convert to long format
  colnames(dirmat)[(length(dirmat)-1):length(dirmat)]<-c("variable", "dir") # make last column "dir"
  dirmat$time<-as.numeric(gsub("X", "", dirmat$variable)) # add time column, drop X resulting from melt
  dirmat$variable<-NULL
  dirmat$wormID<-dirmat$exp:dirmat$condition:dirmat$animal # need to generalize this
  #dirmat$bin<- cut(dirmat$time, seq(0,max(dirmat$time), by = n), dig.lab=10)
  dirmat$dir.bin<-cut(dirmat$dir, c(0,45,135,225,315,360), dig.lab=10,na.rm = TRUE) # assume 0 = to the right
  index<-c("(0,45]", "(45,135]", "(135,225]", "(225,315]", "(315,360]")
  value<-c("right", "up", "left", "down", "right")
  dirmat$direction<-value[match(dirmat$dir.bin, index)]
  dirmat$direction[is.na(dirmat$direction)] <- "right"
  dirmat$direction<-as.factor(dirmat$direction)
  return(data.frame(dirmat))
}

# make 3 objects using above functions in long format
behmat<-for.mat(n) # make DF with time bin of 240 frames = 120 sec
ymat<-y.mat(n)  # ditto for position data
dirmat<-dir.mat(n)

# merge the 3 datasets
library(dplyr)
tot.mat<-left_join(behmat, ymat)
tot.mat<-left_join(tot.mat, dirmat)
tot.mat<-tot.mat[order(tot.mat[,8], tot.mat[,7]), ] # order by wormID, then time

# need to align all events where the worms encounter a stripe +- 2.5mm - this is manual at this point
tot.mat.down1<-tot.mat[tot.mat$direction == "down" & 
                         (tot.mat$pos < (-4.25) & tot.mat$pos > (-9.25)) ,]
tot.mat.down1$norm.pos<-tot.mat.down1$pos+6.75
tot.mat.down2<-tot.mat[tot.mat$direction == "down" & 
                         (tot.mat$pos < (-10.375) & tot.mat$pos > (-15.375)),]
tot.mat.down2$norm.pos<-tot.mat.down2$pos+12.875
tot.mat.down<-merge(tot.mat.down1, tot.mat.down2, all=TRUE)
tot.mat.down$norm.pos<- (-tot.mat.down$norm.pos)

tot.mat.up1<-tot.mat[tot.mat$direction == "up" & 
                         (tot.mat$pos < (-0.7) & tot.mat$pos > (-5.7)) ,]
tot.mat.up1$norm.pos<-tot.mat.up1$pos+3.2
tot.mat.up2<-tot.mat[tot.mat$direction == "up" & 
                         (tot.mat$pos < (-7.17) & tot.mat$pos > (-12.17)),]
tot.mat.up2$norm.pos<-tot.mat.up2$pos+9.67
tot.mat.up<-merge(tot.mat.up1, tot.mat.up2, all=TRUE)
tot.mat.norm<-merge(tot.mat.up, tot.mat.down, all=TRUE)
tot.mat.norm$norm.pos<-round((tot.mat.norm$norm.pos), digits = 1)

# get post-reversal behavior (as column now but should figure better way)
library(data.table)
tot.mat.t<-as.data.table(tot.mat)

rowShift <- function(x, shiftLen = 1L) {
  r <- (1L + shiftLen):(length(x) + shiftLen)
  r[r<1] <- NA
  return(x[r])
}

tot.mat.t[, postRev:= rowShift(state,1)]
tot.mat.t[, postRev2:= rowShift(state,2)]
tot.mat.t[, postRev3:= rowShift(state,3)]
tot.mat.t[, postRev4:= rowShift(state,4)]
tot.mat.t[, postRev5:= rowShift(state,5)]
tot.mat.t[, postRev6:= rowShift(state,6)]
tot.mat.t[, postRev7:= rowShift(state,7)]
tot.mat.t[, postRev8:= rowShift(state,8)]
tot.mat.t[, postRev9:= rowShift(state,9)]
tot.mat.t[, postRev10:= rowShift(state,10)]
tot.mat.t[, postomR := rowShift(state,1)]
tot.mat.t[, postomF := rowShift(state,1)]
tot.mat<-as.data.frame(tot.mat.t)
tot.mat[tot.mat$Rev==0, "postRev"] <- NA
tot.mat[tot.mat$Rev==0, "postRev2"] <- NA
tot.mat[tot.mat$Rev==0, "postRev3"] <- NA
tot.mat[tot.mat$Rev==0, "postRev4"] <- NA
tot.mat[tot.mat$Rev==0, "postRev5"] <- NA
tot.mat[tot.mat$Rev==0, "postRev6"] <- NA
tot.mat[tot.mat$Rev==0, "postRev7"] <- NA
tot.mat[tot.mat$Rev==0, "postRev8"] <- NA
tot.mat[tot.mat$Rev==0, "postRev9"] <- NA
tot.mat[tot.mat$Rev==0, "postRev10"] <- NA
tot.mat[tot.mat$omR==0, "postomR"] <- NA
tot.mat[tot.mat$omF==0, "postomF"] <- NA
# rm(behmat)
# rm(ymat)  # remove these if script works for space

#behprob<-(tapply(behmat$omR, behmat$bin, function (x) {
  #tapply(behmat$omR, behmat$wormID, mean) }))

# combine summary binary data across bins or linear time and space *** could parse columns and make this a function
time_beh<-aggregate(cbind(omF, omR, Rev, For, Pir, Curve, Pau) ~ time + condition + exp, data = behmat, FUN = mean)
bin_beh<-aggregate(cbind(omF, omR, Rev, For, Pir, Curve, Pau) ~ bin + condition + exp + wormID, data = behmat, FUN = mean)
pos_beh<-aggregate(cbind(omF, omR, Rev, For, Pir, Curve, Pau) ~ pos + bin + condition + exp + direction, data = tot.mat, FUN = mean)
pos_bin_beh<-aggregate(cbind(omF, omR, Rev, For, Pir, Curve, Pau) ~ pos.bin + bin + condition + exp + wormID + direction, data = tot.mat, FUN = mean)

norm_timbin_beh<-aggregate(cbind(Pir, For, Rev, Curve) ~ norm.pos + bin + condition + wormID, data = tot.mat.norm, FUN=mean)
norm_pos_beh<-aggregate(cbind(Pir, For, Rev, Curve) ~ norm.pos + condition + wormID, data = tot.mat.norm, FUN = mean)
norm_time_beh<-aggregate(cbind(Pir, For, Rev, Curve) ~ norm.pos + time + condition + wormID, data = tot.mat.norm, FUN = mean)

##### bunch of plotting below
# plot total state probs with smoothing
plot1<-ggplot(time_beh, aes(x=time)) +
  geom_smooth(aes(y=Pir, linetype=condition), level=0.99, colour="blue") +
  geom_smooth(aes(y=Rev, linetype=condition), level=0.99,colour="red") +
  geom_smooth(aes(y=Pau, linetype=condition), level=0.99,colour="yellow") + 
  geom_smooth(aes(y=For, linetype=condition), level=0.99,colour="green") +
  coord_cartesian(ylim=c(0, 1)) + 
  theme_my

# plot Pirouette state prob in bins by animal
plot2<-ggplot(bin_beh, aes(x=bin, y=Pir)) + 
  geom_boxplot(aes(colour=condition)) +
  theme_my

#plot states by x pos uses gam for curve fit #comment states you don't want to plot
library(splines)
library(scales)
plot3<-ggplot(subset(pos_beh, direction == "up" | direction == "down"), aes(x=pos)) +
  annotate("rect", 0,0,-9.67,-6.75,0,1, fill="lightblue", alpha=0.3)  +
  annotate("rect", 0,0,-3.2,0,0,1, fill="lightblue", alpha=0.3)  + 
  annotate("rect", 0,0,-15.7,-12.875,0,1, fill="lightblue", alpha=0.3)  + 
  geom_smooth(aes(y=Pir, linetype=condition), colour="blue") +
  #geom_smooth(aes(y=omR, linetype=condition), colour="magenta") +
  geom_smooth(aes(y=Rev, linetype=condition), colour="red") +
  geom_smooth(aes(y=Curve, linetype=condition), colour="orange") +
  #geom_smooth(aes(y=Pau, linetype=condition), level=0.99,colour="yellow") + 
  geom_smooth(aes(y=For, linetype=condition), level=0.99,colour="green") +
  #coord_cartesian(ylim=c(0, 1)) +
  facet_grid(exp~direction) + 
  theme_my

boundaries<-c(0,3.2, 6.75, 9.67, 12.875)
#plot pirouettes by x pos bin

plot4<-ggplot(norm_pos_beh,
              aes(x=norm.pos)) + 
  annotate("rect", 0,0,0,1.5,0,1, fill="lightblue", alpha=0.2) +
  geom_smooth(aes(y=Pir, linetype=condition), alpha = 0.2, colour="blue") +
  geom_smooth(aes(y=Rev, linetype = condition), alpha=0.2,colour="red") + 
  geom_smooth(aes(y=Curve, linetype = condition), alpha=0.2, colour="purple") + 
  geom_smooth(aes(y=For, linetype = condition), alpha=0.2, colour = "darkgreen") +
  coord_cartesian(xlim=c(-1.5,1.5), ylim=c(-.05, 0.6)) + theme_my +
  facet_wrap(~bin)

# takes a ton of memory!
plot5<-ggplot(tot.mat.norm, aes(x = norm.pos)) + 
  annotate("rect", 0,0,0,1.5,0,1, fill="lightblue", alpha=0.2) +
  geom_smooth(aes(y=Pir, linetype=condition), method=loess, span = 0.1, alpha = 0.15, colour="blue") +
  geom_smooth(aes(y=Rev, linetype = condition), method=loess, span = 0.1,alpha=0.15,colour="red") + 
  geom_smooth(aes(y=Curve, linetype = condition), method=loess, span = 0.1, alpha=0.15, colour="purple") + 
  geom_smooth(aes(y=For, linetype = condition), method=loess, span = 0.1, alpha=0.15, colour = "darkgreen") +
  coord_cartesian(xlim=c(-1.5,1.5), ylim=c(-.05, 0.65)) + theme_my 
ggplotly(plot5)

#plot post-rev behavior: 
plot6<-ggplot(tot.mat[!is.na(tot.mat$postomF),], aes(x=pos))
  
plot6 + geom_bar(aes(fill=postomF)) +
  facet_grid(condition ~ direction)

# for a useless mean +- SEM bargraph
with(pos_bin_beh, bargraph.CI(x.factor=pos.bin,  #categorical factor for the x-axis
  group=condition,                           
  response=For,   #numerical DV for the y-axis
  legend=T,
  cex.leg=0.8,
  x.leg=0,
  y.leg=-0.025,
  ncol=2,
  ylab="chemotaxis index",
  xlab="strain"))
  #ylim=c(-.05,0.6)))
  

plot1
plot2
plot3
plot4


####### below is testing area #######
# 
# Pirprob<-aggregate(Pir ~ bin + wormID + condition + exp, data = behmat, FUN = mean)
# Pirplot<-ggplot(Pirprob, aes(x=bin, y=Pir, group=condition)) + 
#   geom_point(aes(colour=condition),position=position_dodge(width=0.75), alpha=0.4) +
#   scale_color_manual(name="bacterial strain", values = c(JU54 = "#56B4E9", OP50 = "#E69F00")) +
#   coord_cartesian(ylim=c(0, 1)) + 
#   theme_my +
#   theme(axis.text.x=element_blank())
# Pirplot
# ggplotly(Pirplot)
# 
# Pirtime<-aggregate(Pir ~ time + wormID + condition + exp, data = behmat, FUN = mean)
# PirtimePlot<-ggplot(Pirtime, aes(x=time, y=Pir, group=condition)) + 
#   geom_smooth(aes(colour=condition)) +
#   scale_color_manual(name="bacterial strain", values = c(JU54 = "#56B4E9", OP50 = "#E69F00")) +
#   coord_cartesian(ylim=c(0, 1)) + 
#   theme_my +
#   theme(axis.text.x=element_blank())
# PirtimePlot
# ggplotly(PirtimePlot)
# 
# #library(splines) for polynomial geom_smooth
# #library(MASS)
# 
# 
# #by(behmat[,c(1,2,8)], behmat[,"omF"], summary)
# 
# #####
# # result of outlier preds:
# 
# #conditionJU54:exp3:animal4   -2.944142   0.355224  -8.288  < 2e-16 ***
# #conditionJU54:exp1:animal6   -3.567111   0.514959  -6.927 4.30e-12 ***
# #conditionJU54:exp3:animal17 -13.165574  52.444371  -0.251 0.801784 
# #conditionOP50:exp1:animal20 -13.165542 151.719854  -0.087 0.930850 
# #conditionJU54:exp3:animal38  -4.286168   0.717751  -5.972 2.35e-09 ***
# #drop these
# behmat<-subset(behmat, behmat$wormID!='3:JU54:4')
# behmat<-subset(behmat, behmat$wormID!='1:JU54:6')
# behmat<-subset(behmat, behmat$wormID!='3:JU54:17')
# behmat<-subset(behmat, behmat$wormID!='1:OP50:20')
# behmat<-subset(behmat, behmat$wormID!='3:JU54:38')
# 
# 
# #behmat<-subset(behmat, behmat$state!='NaN') 
# 
# 
# 
# ## make state factor for analysis
# behmat$state<-as.factor(behmat$state) 
# 
# #####
# library(ggplot2)
# library(scales)
# #p<-ggplot(na.omit(behmat), aes(time,..density.., colour=state))
# #p + geom_density(binwidth=2400) + facet_grid(.~condition)
# 
# p1<-ggplot(behmat, aes(x=time, fill=state))
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# p1 + geom_histogram(position='fill', binwidth=10) + facet_grid(~condition) +
#   scale_fill_manual(values=cbPalette)
# #p1 + geom_histogram(aes(y=..density..)) + facet_grid(state~condition) +
# #  geom_density(aes(colour=state))  +scale_fill_brewer()
# 
# 
# #behmat$state <- relevel(behmat$state, ref = 8)
# 
# ### for multinomial logistic regression
# #library(nnet)
# #mod<-multinom(state~condition, data=behmat)
# #summary(mod) - huh? makes no sense
# 
# #library(VGAM)
# #mod <- vglm(state ~ condition + time, family=multinomial(refLevel=1), data=behmat) # plot states
# 
# #####
# 
# omFmod<-glm(omF~condition*time, family=binomial, data=behmat) # glm for omega Forward
# omRmod<-glm(omR~condition*time, family=binomial, data=behmat) # glm for omega Rev
# Formod<-glm(For~condition*time, family=binomial, data=behmat) # glm for Forward run
# Paumod<-glm(Pau~condition*time, family=binomial, data=behmat) # glm for pause - no effect
# Revmod<-glm(Rev~condition*time, family=binomial, data=behmat) # glm for reversals
# Pirmod<-glm(Pir~condition*time, family=binomial, data=behmat) # glm for total pirouettes
# 
# rm(testmod)<-glm(omF~exp*condition + time, family=binomial, data=behmat)
# test2mod<-glm(omF~exp*condition*time, family=binomial, data=behmat)
# 
# # or use glmer
# #library(lme4)
# omF.glmm<-glmer(omF~condition*bin + (1|exp),
#                 family=binomial, data=behmat,  control=glmerControl(optimizer="bobyqa"))
# 
# #outliers<-glm(omF~wormID, data=behmat)
# ########
# 
# newdata<-expand.grid(condition=c('JU54', 'OP50'), time=seq(1,2400, by=10))
# newdata$omF<-predict(omFmod, type = "response", newdata=newdata)
# newdata$omR<-predict(omRmod, type = "response", newdata=newdata)
# newdata$For<-predict(Formod, type = "response", newdata=newdata)
# newdata$Pau<-predict(Paumod, type = "response", newdata=newdata)
# newdata$Rev<-predict(Revmod, type = "response", newdata=newdata)
# newdata$Pir<-predict(Pirmod, type = "response", newdata=newdata)
# 
# # get SE for each fithead()
# omFpreds<-predict(omFmod, newdata = newdata, type = 'response',se = TRUE)
# omRpreds<-predict(omRmod, newdata = newdata, type = 'response',se = TRUE)
# Forpreds<-predict(Formod, newdata = newdata, type = 'response',se = TRUE)
# Paupreds<-predict(Paumod, newdata = newdata, type = 'response',se = TRUE)
# Revpreds<-predict(Revmod, newdata = newdata, type = 'response',se = TRUE)
# Pirpreds<-predict(Pirmod, newdata = newdata, type = 'response',se = TRUE)
# 
# newdata$omFmin<-newdata$omF-omFpreds$se.fit
# newdata$omFmax<-newdata$omF+omFpreds$se.fit
# newdata$omRmin<-newdata$omR-omRpreds$se.fit
# newdata$omRmax<-newdata$omR+omRpreds$se.fit
# newdata$Formin<-newdata$For-Forpreds$se.fit
# newdata$Formax<-newdata$For+Forpreds$se.fit
# newdata$Paumin<-newdata$Pau-Paupreds$se.fit
# newdata$Paumax<-newdata$Pau+Paupreds$se.fit 
# newdata$Revmin<-newdata$Rev-Revpreds$se.fit
# newdata$Revmax<-newdata$Rev+Revpreds$se.fit  
# newdata$Pirmin<-newdata$Pir-Pirpreds$se.fit
# newdata$Pirmax<-newdata$Pir+Pirpreds$se.fit 
# 
# # get prediction intervals for each model
# 
# #####
# p2<-ggplot(newdata, aes(x=time, y=omF, group=condition)) + 
#   geom_ribbon(aes(y=omF, ymin=omFmin, ymax=omFmax), alpha=0.2) +
#   geom_line(data=newdata, aes(y=omF, colour=condition)) +
#   scale_color_manual(name="bacterial strain", values = c(JU54 = "#56B4E9", OP50 = "#E69F00"))+ 
#   ylab('pirouette forward probability')
# p2
# 
# p3<-ggplot(newdata, aes(x=time, y=omR, group=condition)) +
#   geom_ribbon(aes(y=omR, ymin=omRmin, ymax=omRmax), alpha=0.2) + 
#   geom_line(data=newdata, aes(y=omR, colour=condition)) +
#   scale_color_manual(name="bacterial strain", values = c(JU54 = "#56B4E9", OP50 = "#E69F00"))+ 
#   ylab('pirouette reverse probability')
# p3
# 
# p3a<-ggplot(newdata, aes(x=time, y=Pir, group=condition)) + 
#   geom_ribbon(aes(y=Pir, ymin=Pirmin, ymax=Pirmax), alpha=0.2) + 
#   geom_line(data=newdata, aes(y=Pir, colour=condition)) +
#   geom_smooth(method="loess") + 
#   scale_color_manual(name="bacterial strain", values = c(JU54 = "#56B4E9", OP50 = "#E69F00"))+
#   coord_cartesian(ylim=c(0, 1)) +
#   ylab('pirouette probability') +
#   theme_my
# p3a
# 
# library(lubridate)
# library(gridExtra)
# theme_my <- theme_bw() + theme(
#   axis.line        = element_line(colour = "black"),
#   panel.grid.major = element_blank(), 
#   panel.grid.minor = element_blank(),
#   panel.border     = element_blank(),
#   strip.background = element_blank(),
#   legend.key       = element_blank(), 
#   axis.text.x=element_text(angle=45, hjust=1, size=12)
# )
# 
# grid.newpage()
# grid.draw(rbind(ggplotGrob(Pirplot), ggplotGrob(p3a), size = "last"))
# 
# p4<-ggplot(newdata, aes(x=time, y=For, group=condition))
# p4 + coord_cartesian(ylim=c(0,0.7)) +
#   facet_grid(exp~.) + 
#   geom_smooth(method=glm, formula=y~x,family="binomial", data=behmat, aes(group=condition)) + 
#   geom_smooth(method=lm, formula=y~x) + 
#   ylab('Forward run probability') +  
#   geom_ribbon(aes(y=For, ymin=Formin, ymax=Formax), alpha=0.2)
# 
# p5<-ggplot(newdata, aes(x=time, y=Pau, colour=condition))
# p5 + 
#   coord_cartesian(ylim=c(0,0.2)) +
#   geom_smooth(method=glm, family="binomial", data=behmat) +
#   ylab('Forward run probability')
# 
# 
# 
# 
# rm(mod) # plot prediction intervals

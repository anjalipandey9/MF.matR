#' beh_mat melt function
#' Takes a matlab behmat file and converts it to long format
#' plus it assigns binary state columns to each behavioral state class
#' @param n number of time frames to bin. generally 400 for 20 minute video is standard
#' @export
#' @examples
#' beh_mat()

beh_mat<- function (n) {
  environment<-globalenv()
  behmat<-read.csv(tk_choose.files(caption = "Choose behmat file"))
  id.cols<-c(colnames(behmat[1:(length(behmat)-2400)])) # use all but state columns as IDs
  behmat[, id.cols] <- lapply(behmat[,id.cols], factor) # convert all ID columns to factors
  behmat<-melt(behmat, id.vars=id.cols) # convert to long
  colnames(behmat)[(length(behmat)-1):length(behmat)]<-c("variable", "state") # make last column "state"
  behmat$time<-as.numeric(gsub("[^0-9]","", behmat$variable)) # add time column, drop X resulting from melt
  behmat$variable<-NULL
  behmat$pixelSize<-as.numeric(as.character(behmat$pixelSize))
  behmat<-subset(behmat, behmat$state<7) # eliminate untracked #7/8
  behmat$wormID<-behmat$genotype:behmat$exp:behmat$condition:behmat$animal # need to generalize this
  behmat$bin<- cut(behmat$time, seq(0,max(behmat$time), by = n), dig.lab=10)
  behmat$omR[behmat$state == 5] <- 1 ## omega reverse = 1 for glm
  behmat$omR[is.na(behmat['omR'])]<-0 ## all else = 0
  behmat$omF[behmat$state == 6] <- 1 ## omega forward = 1 for glm
  behmat$omF[is.na(behmat['omF'])]<-0 ## all else = 0
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

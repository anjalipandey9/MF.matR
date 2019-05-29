#' y_mat melt function
#' Takes a matlab ymat file and converts it to long format for further analysis
#'
#' @param bins number of time frames to bin. generally 400 for 20 minute video is standard
#' @export
#' @examples
#' y_mat()

y_mat <- function (bins) {
  environment<-globalenv()
  message("Choose ymat file")
  ymat<-read.csv(file.choose())
  id.cols<-c(colnames(ymat[1:(length(ymat)-2400)])) # use all but y columns as IDs
  ymat[, id.cols] <- lapply(ymat[,id.cols], factor) # convert all ID columns to factors
  ymat<-melt(ymat, id.vars=id.cols) # convert to long
  colnames(ymat)[(length(ymat)-1):length(ymat)]<-c("variable", "y") # make last column "y" - converted from "pos"
  ymat$pixelSize<-as.numeric(as.character(ymat$pixelSize))
  ymat$y<-round(-(ymat$y)/(ymat$pixelSize), digits = 1) # convert pixels to 16 mm
  #ymat$time<-as.numeric(gsub("[^0-9]","", ymat$variable)) # add time column, drop X resulting from melt
  ymat$variable<-NULL
  ymat$wormID<-ymat$genotype:ymat$exp:ymat$condition:ymat$animal # need to generalize this
  #ymat$bin<- cut(ymat$time, seq(0,max(ymat$time), by = n), dig.lab=10)
  ymat$y.bin<-cut(ymat$y, 30, dig.lab=10)
  return(data.frame(ymat))
}

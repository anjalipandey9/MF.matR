#' dir_mat melt function
#' Takes a matlab dirmat file and converts it to long format
#'
#' @param n number of time frames to bin. generally 400 for 20 minute video is standard
#' @export
#' @examples
#' dir_mat()

dir_mat <- function (n) {
  environment<-globalenv()
  dirmat<-read.csv(tk_choose.files(caption = "Choose dirmat file"))
  id.cols<-c(colnames(dirmat[1:(length(dirmat)-2400)])) # identify all non time columns
  dirmat[, id.cols] <- lapply(dirmat[,id.cols], factor) # convert all ID columns to factors
  dirmat<-melt(dirmat, id.vars=id.cols) # convert to long format
  colnames(dirmat)[(length(dirmat)-1):length(dirmat)]<-c("variable", "dir") # make last column "dir"
  dirmat$time<-as.numeric(gsub("[^0-9]","", behmat$variable)) # add time column, drop X resulting from melt
  dirmat$variable<-NULL
  dirmat$pixelSize<-as.numeric(as.character(dirmat$pixelSize))
  dirmat$wormID<-dirmat$genotype:dirmat$exp:dirmat$condition:dirmat$animal
  #dirmat$bin<- cut(dirmat$time, seq(0,max(dirmat$time), by = n), dig.lab=10)
  dirmat$dir.bin<-cut(dirmat$dir, c(0,45,135,225,315,360), dig.lab=10,na.rm = TRUE) # assume 0 = to the right
  index<-c("(0,45]", "(45,135]", "(135,225]", "(225,315]", "(315,360]")
  value<-c("right", "up", "left", "down", "right")
  dirmat$direction<-value[match(dirmat$dir.bin, index)]
  #dirmat$direction[is.na(dirmat$direction)] <- "right" fix this later in tot.mat
  dirmat$direction<-as.factor(dirmat$direction)
  return(data.frame(dirmat))
}

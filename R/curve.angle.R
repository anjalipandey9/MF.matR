#' function to calculate 3 point curvature based on law of cosines
#' function is intended to be used with default arguments within a Wormlab data structure
#' @param del.x1 change from previous point (t-1) to (t0)
#' @param del.y1 change from previous point (t-1) to (t0)
#' @param del.x2 change from t(-2) to t(-1)
#' @param del.y2 change from t(-2) to t(-1)
#' @export
#' @examples
#' WL.alldata %>% mutate(curve.ang = as.numeric(mapply(MF.matR::curve.angle, del.x1, del.y1, del.x2, del.y2))*180/pi))
curve.angle <- function(del.x1, del.y1, del.x2, del.y2) {
  values <- list(del.x1, del.y1, del.x2, del.y2)
  if(anyNA(values)) {
    "NA"
  } else {
    x <- c(del.x1, del.y1)
    y <- c(del.x2, del.y2)
    dot.prod <- x%*%y
    norm.x <- as.numeric(svd(x)[1]) # faster to use svd and index (which for 1x1 vec is all norm does)
    #norm(x,type="2") # length of vector
    norm.y <- as.numeric(svd(y)[1])
    #norm(y,type="2")
    theta <- acos(dot.prod / (norm.x * norm.y))
    as.numeric(theta)
  }
}

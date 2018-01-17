#' read_binsize function
#' simple prompt for binsize - user input
#'
#' @param n empty input - will take user input
#' @export
#' @examples
#' n <- read_binsize()

read.binsize <- function() {
  n <- readline(prompt="Enter length of time bins: ")
  if(!grepl("^[0-9]+$",n))
  {
    return(read.binsize())
  }
  return(as.integer(n))
}

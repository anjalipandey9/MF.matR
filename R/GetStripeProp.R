#' using all_track_data file, determines whether a worm is in stripe (1) or out (0)
#' based on an ideal, half-width stripe
#' @param data input data of type all_track_data from Import.WL.data()
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
#' @examples
#' data %>% GetStripeProp() -> data

GetSripeProp <- function(data) {
  max <- max(data$y)
  min <- min(data$y)
  delta  <- max-min
  bounds <- c(max, max - (delta/4), min + (delta/4), min)

  data %>% mutate(
    InStripe = case_when(
      y > bounds[2] ~ 0,
      y < bounds[3] ~ 0,
      TRUE ~ 1
    )
  )
}

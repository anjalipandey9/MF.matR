#' using all_track_data.csv file, calculates proportion of worm tracks in (1) or out (0) of a stripe
#' uses GetStripeProp to determine whether tracks are in or out
#' Writes the proportion data to a csv within the same folder.
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
#' @examples
#' WriteStripeProp()

WriteStripeProp <- function(...) {
  filename <- file.choose()
  file.pref <- basename(filename)
  folder <- dirname(filename)
  data <- read.csv(filename)

  data %>% GetSripeProp() %>%
    summarise(prop = sum(InStripe)/nrow(.),
              filename = file.pref) -> data

  data.table::fwrite(data, file.path(folder,paste0("prop_in_stripe.csv")))
  }

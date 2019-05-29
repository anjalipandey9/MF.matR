#' beh_mat_tidy
#' Takes a matlab behmat file and converts it to long format
#' plus it assigns binary state columns to each behavioral state class
#' @param bins number of time frames to bin. generally 400 for 20 minute video is standard
#' @param frames number of frames in the video
#' @export
#' @examples
#' beh_mat_tidy


beh_mat_tidy <- function(bins, frames) {
  message("Choose behmat file")
  behmat<-readr::read_csv(file.choose()) %>%
    tidyr::gather(key = "time", value = "state", tidyselect::starts_with("t")) %>% #put all data columns (time = "t1 ... "t2400") into long format
   dplyr::mutate(
  time = as.numeric(gsub("[^0-9]", "", time)),
  wormID = interaction(genotype, exp, condition, animal),
  state_name = dplyr::case_when(
    #!state %in% c(1:8) ~ "NA",
    state == "1" ~ "Forward",
    state == "2" ~ "Curve",
    state == "3" ~ "Reverse",
    state == "4" ~ "Pause",
    state == "5" ~ "Omega-R",
    state == "6" ~ "Omega-F",
    state %in% c("7", "8") ~ "no_state_call",
    TRUE ~ "NA"
  )
)
}

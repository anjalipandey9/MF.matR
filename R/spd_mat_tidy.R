#' spd_mat_tidy data munge
#' Takes a matlab ymat file and converts it to long format for further analysis
#'
#' @param bins number of time frames to bin. generally 400 for 20 minute video is standard
#' @export
#' @examples
#' spd_mat_tidy()

spd_mat_tidy <- function () {
  spdmat<-readr::read_csv(file.choose()) %>%
    tidyr::gather(key = "time", value = "speed", tidyselect::starts_with("t")) %>% #put all data columns (time = "t1 ... "t2400") into long format
    dplyr::mutate(time = as.numeric(gsub("[^0-9]","", time)),
                  speed = round(speed/pixelSize, digits = 2))
}

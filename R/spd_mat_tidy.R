#' spd_mat_tidy data munge
#' Takes a matlab ymat file and converts it to long format for further analysis
#'
#' @param bins number of time frames to bin. generally 400 for 20 minute video is standard
#' @export
#' @examples
#' spd_mat_tidy()

spd_mat_tidy <- function (interactive = TRUE) {
  if(interactive) {
    message("Choose spdmat file")
    xmat<-read_csv(file.choose(), col_names = FALSE)
    message("choose preprocess file")
    preprocess <- readr::read_csv(file.choose(), col_names = TRUE) %>%
      select(pixelSize)
  } else {
    spdPath <- fs::dir_ls(folder, regex = "spdmat")
    preprocessPath <- fs::dir_ls(folder, regex = "preprocess.csv")
    spdmat <- readr::read_csv(spdPath, col_names = FALSE)
    pixelSize <- readr::read_csv(preprocessPath, col_names = TRUE) %>%
      select(pixelSize) %>% as.numeric()
  }


  spdmat <- spdmat %>%
    mutate(worm = factor(seq(1:nrow(spdmat)))) %>%
    select(worm, everything()) %>%
    tidyr::gather(key = "time", value = "speed", tidyselect::starts_with("X")) %>% #put all data columns (time = "t1 ... "t2400") into long format
    dplyr::mutate(time = as.numeric(gsub("[^0-9]","", time)),
                  speed = round(speed/pixelSize, digits = 2))
}

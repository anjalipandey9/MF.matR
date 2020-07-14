#' y_mat_tidy data munge
#' Takes a matlab ymat file and converts it to long format for further analysis
#'
#' @param bins number of time frames to bin. generally 400 for 20 minute video is standard
#' @export
#' @examples
#' y_mat_tidy()

y_mat_tidy <- function (interactive = TRUE) {
  message("Choose ymat file")


  if(interactive) {
    message("Choose ymat file")
    ymat<-read_csv(file.choose(), col_names = FALSE)
    message("choose preprocess file")
    preprocess <- readr::read_csv(file.choose(), col_names = TRUE) %>%
      select(pixelSize)
  } else {
    ymatPath <- fs::dir_ls(folder, regex = "ymat")
    preprocessPath <- fs::dir_ls(folder, regex = "preprocess.csv")
    ymat <- readr::read_csv(ymatPath, col_names = FALSE)
    pixelSize <- readr::read_csv(preprocessPath, col_names = TRUE) %>%
      select(pixelSize) %>% as.numeric()
  }
  ymat %>%
    mutate(worm = factor(seq(1:nrow(ymat)))) %>%
    select(worm, everything()) %>%
    tidyr::gather(key = "time", value = "y", tidyselect::starts_with("X")) %>% #put all data columns (time = "t1 ... "t2400") into long format
    dplyr::mutate(time = as.numeric(gsub("[^0-9]","", time)),
                  y = round(-y/pixelSize, digits = 2))
}

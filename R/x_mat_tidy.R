#' x_mat_tidy data munge
#' Takes a matlab xmat file and converts it to long format for further analysis
#'
#' @param bins number of time frames to bin. generally 400 for 20 minute video is standard
#' @export
#' @examples
#' x_mat_tidy()

x_mat_tidy <- function (interactive = TRUE) {
  message("Choose xmat file")


  if(interactive) {
    message("Choose xmat file")
    xmat<-read_csv(file.choose(), col_names = FALSE)
    message("choose preprocess file")
    preprocess <- readr::read_csv(file.choose(), col_names = TRUE) %>%
      select(pixelSize)
  } else {
    xmatPath <- fs::dir_ls(folder, regex = "xmat")
    preprocessPath <- fs::dir_ls(folder, regex = "preprocess.csv")
    xmat <- readr::read_csv(xmatPath, col_names = FALSE)
    pixelSize <- readr::read_csv(preprocessPath, col_names = TRUE) %>%
      select(pixelSize) %>% as.numeric()
  }
  xmat %>%
    mutate(worm = factor(seq(1:nrow(xmat)))) %>%
    select(worm, everything()) %>%
    tidyr::gather(key = "time", value = "x", tidyselect::starts_with("X")) %>% #put all data columns (time = "t1 ... "t2400") into long format
    dplyr::mutate(time = as.numeric(gsub("[^0-9]","", time)),
                  x = round(-x/pixelSize, digits = 2))
}

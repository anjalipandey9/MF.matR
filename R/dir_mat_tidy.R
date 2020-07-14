#' dir_mat_tidy melt function
#' Takes a matlab-derived dirmat .csv file and converts it to long format
#'
#' @export
#' @examples
#' dir_mat_tidy()

dir_mat_tidy <- function (interactive = TRUE) {

  if(interactive) {
    message("Choose dirmat file")
    dirmat<-read_csv(file.choose(), col_names = FALSE)
  } else {
    dirmatPath <- fs::dir_ls(folder, regex = "dirmat")
    dirmat <- readr::read_csv(dirmatPath, col_names = FALSE)
  }

  dirmat <- dirmat %>%
    mutate(worm = factor(seq(1:nrow(dirmat)))) %>%
    select(worm, everything()) %>%
    tidyr::gather(key = "time", value = "dir", tidyselect::starts_with("X")) %>% #put all data columns (time = "t1 ... "t2400") into long format
    dplyr::mutate(time = as.numeric(gsub("[^0-9]","", time)),
                  dir.bin = cut(dir, c(0,45,135,225,315,360), dig.lab=10, na.rm = TRUE),
                  direction = dplyr::case_when(
                    dir.bin %in% c("(0,45]", "(315,360]") ~ "right",
                    dir.bin == "(45,135]" ~ "up",
                    dir.bin == "(135,225]" ~ "left",
                    dir.bin == "(225,315]" ~ "down"
                  ))
}

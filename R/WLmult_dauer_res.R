#' Wrapper for 'Import.WL.data' and 'plot_Residency' to allow multiple datasets to be simultaneously
#' analyzed. Uses recursive search for a *position.csv file, then makes a file list.
#' @param bin.length length of time bins in seconds. Used for state analysis
#' @param frame.rate video frame rate
#' @param num.tracks optional argument to limit input to certain number of worm tracks
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
#' @examples
#' WLmult_dauer_res()

WLmult_dauer_res <- function() {
  directory <- dirname(file.choose())
  file_list <- fs::dir_ls(directory, recursive = TRUE, glob = "*osition.csv")
  safe_WLimport <- purrr::possibly(Import.WL.data, otherwise = "error - prob missing data")
  file_list %>% map(., function(x) {
    mult_pos_file <<- x
    safe_WLimport(bin.length = 2, frame.rate = 6, multiple = TRUE)
  })

  file_list2 <- fs::dir_ls(directory, recursive = TRUE, glob = "*all_track_data.csv")
  safe_WLres <- purrr::possibly(plot_Residency, otherwise = "error - prob missing data")
  file_list2 %>% map(., function(x) {
    mult_track_file <<- x
    safe_WLres(time_bin = 4, y_bin = 100, overlay = TRUE, multiple = TRUE)
  })
}



#' Plot state probability from Wormlab dauer data by space and time.
#' @param df data frame
#' @param time_bin number of bins to split plot
#' @param y_bin number of position bins
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
#' @examples
#' plot_stateProbs <-
#'

plot_stateProbs <- function(df, time_bin, y_bin, nopause) {
  if(!suppressWarnings(
    suppressPackageStartupMessages(
      require(patchwork,
              quietly = TRUE,
              character.only = TRUE)))) {
    devtools::install_github("thomasp85/patchwork")
    suppressPackageStartupMessages(library(patchwork,character.only = TRUE))
  }
  devtools::install_github("")
  if (missing(nopause)) {
  df %>% dplyr::ungroup() %>%
      dplyr::mutate(time.bin = dplyr::ntile(Time,time_bin),
           y.bin = dplyr::ntile(y,y_bin),
           state = factor(state)) %>%
    dplyr::count(time.bin,y.bin,state) %>%
    dplyr::group_by(time.bin,y.bin) %>%
    dplyr::mutate(prop = prop.table(n)) %>%
    ggplot(aes(x = y.bin, y = prop)) +
    geom_point(aes(color = state)) + geom_smooth(aes(group = state, colour = state)) +
      facet_wrap(~time.bin)
  } else {
    df %>% dplyr::ungroup() %>%
      dplyr::mutate(time.bin = factor(dplyr::ntile(Time,time_bin)),
                    y.bin = dplyr::ntile(y,y_bin),
                    state = factor(state)) %>%
      dplyr::count(time.bin,y.bin,state) %>%
      dplyr::group_by(time.bin,y.bin) %>%
      dplyr::mutate(prop = prop.table(n)) %>%
      dplyr::filter(state != "pause") %>%
      ggplot(aes(x = y.bin, y = prop)) +
      geom_point(aes(color = state)) +
      geom_smooth(aes(group = state, colour = state)) +
      facet_wrap(~time.bin)
  }

}


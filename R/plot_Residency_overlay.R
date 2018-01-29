#' Plot residency from Wormlab dauer data by space and time, overlaying density by time_bin
#' @param df data frame
#' @param time_bin number of bins to split plot
#' @param y_bin number of position bins
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
#' @examples
#' plot_Residency(time_bin = 4, y_bin = 100, nopause = TRUE)

plot_Residency_overlay <- function(df, time_bin, y_bin, nopause) {
  file <- file.choose()
  folder <- dirname(file)
  file.pref <- basename(file) %>% strsplit(., "all_track_data.csv") %>% unlist()
  devtools::install_github("thomasp85/patchwork")
  devtools::install_github("dgrtwo/gganimate")
  library(ggplot2)
  library(patchwork)
  df <- read.csv(file) %>% dplyr::ungroup() %>%
    dplyr::mutate(time.bin = factor(dplyr::ntile(Time,time_bin)),
                  y.bin = dplyr::ntile(y,y_bin),
                  state = factor(state))
  #   dplyr::count(time.bin,y.bin,state) %>%
  #   dplyr::group_by(time.bin,y.bin) %>%
  #   dplyr::mutate(prop = prop.table(n)) %>%
  p2 <- df %>% dplyr::filter(state != "pause") %>%
    ggplot(aes(x = y.bin, colour = time.bin)) +
    #geom_histogram(fill = "grey", bins = y_bin, alpha = 0.2, aes(colour = time.bin)) +
    geom_density(aes(y = ..count..,fill = time.bin), alpha = 0.2, ) +
    theme_classic() +
    #facet_wrap(~time.bin) +
    labs(title = "Mobile Fraction") +
    viridis::scale_color_viridis(discrete = TRUE) +
    viridis::scale_fill_viridis(discrete = TRUE)

  ggsave(file.path(folder,paste0(file.pref[1],"residency_overlay.pdf")), device = "pdf", width = 6, height = 4)

  #p2 <- gganimate::gganimate(p2, interval = 0.1)

}

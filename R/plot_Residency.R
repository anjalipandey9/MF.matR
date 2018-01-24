#' Plot residency from Wormlab dauer data by space and time.
#' @param df data frame
#' @param time_bin number of bins to split plot
#' @param y_bin number of position bins
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
#' @examples
#' read.csv("file") %>% plot_Residency(time_bine = 4, y_bin = 100, nopause = TRUE)
#'

plot_Residency <- function(df, time_bin, y_bin, nopause) {
    # if(!suppressWarnings(
    #   suppressPackageStartupMessages(
    #     require(patchwork,
    #             quietly = TRUE,
    #             character.only = TRUE)))) {
    #   devtools::install_github("thomasp85/patchwork")
    #   suppressPackageStartupMessages(library(patchwork,character.only = TRUE))
    # }

  file <- file.choose()
  folder <- dirname(file)
  file.pref <- basename(file) %>% strsplit(., "all_track_data.csv") %>% unlist()
      devtools::install_github("thomasp85/patchwork")
      library(patchwork)
      df <- read.csv(file) %>% dplyr::ungroup() %>%
        dplyr::mutate(time.bin = dplyr::ntile(Time,time_bin),
                      y.bin = dplyr::ntile(y,y_bin),
                      state = factor(state))
        #   dplyr::count(time.bin,y.bin,state) %>%
        #   dplyr::group_by(time.bin,y.bin) %>%
        #   dplyr::mutate(prop = prop.table(n)) %>%
      p1 <- ggplot(df, aes(x = y.bin)) +
        geom_histogram(colour = "grey",fill = "white", bins = y_bin) +
        geom_density(aes(y = ..count..)) +
        facet_wrap(~time.bin) +
        theme_classic() +
        labs(title = "All Tracks") + guides(color = FALSE)
      p2 <- df %>% filter(state != "pause") %>%
        ggplot(aes(x = y.bin)) +
        geom_histogram(fill = "grey", bins = y_bin) +
        geom_density(aes(y = ..count..)) +
        facet_wrap(~time.bin) +
        theme_classic() +
        labs(title = "Mobile Fraction")

      p3 <- p1 + p2

      ggsave(file.path(folder,paste0(file.pref[1],"residency.pdf")), device = "pdf", width = 6, height = 4)

  }

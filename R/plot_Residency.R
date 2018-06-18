#' Plot residency from Wormlab dauer data by space and time.
#' @param time_bin number of bins to split plot
#' @param y_bin number of position bins
#' @param overlay optional argument to plot overlay of density for 2 time bins
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
#' @examples
#' plot_Residency(time_bin = 4, y_bin = 100, nopause = TRUE)
#'

plot_Residency <- function(time_bin, y_bin, overlay, multiple, stripe.index = FALSE, ...) {
    # if(!suppressWarnings(
    #   suppressPackageStartupMessages(
    #     require(patchwork,
    #             quietly = TRUE,
    #             character.only = TRUE)))) {
    #   devtools::install_github("thomasp85/patchwork")
    #   suppressPackageStartupMessages(library(patchwork,character.only = TRUE))
    # }
if(missing(multiple)) {
  filename <- file.choose()
} else {
  filename <- mult_track_file
}

folder <- dirname(filename)
file.pref <- basename(filename) %>% strsplit(., "all_track_data.csv") %>% unlist()
#devtools::install_github("thomasp85/patchwork")
library(ggplot2)
library(patchwork)
message("reading in data")
df <- read.csv(filename) %>%
  #dplyr::ungroup() %>%
  dplyr::mutate(
    time.bin = cut(Time, time_bin),
    dens.time.bin = factor(cut(Time, 2)),
    y.bin = cut(y, y_bin),
    y_numeric = as.numeric(y.bin),
    state = factor(state)
  )
message("plotting histograms")
p1 <- ggplot(df, aes(x = y_numeric)) +
  geom_histogram(colour = "grey", fill = "white", bins = y_bin) +
  geom_density(aes(y = ..count..)) +
  facet_wrap(~time.bin) +
  theme_classic() +
  labs(title = "All Tracks") + guides(color = FALSE)
p2 <- df %>%
  dplyr::filter(state != "pause") %>%
  ggplot(aes(x = y_numeric)) +
  geom_histogram(fill = "grey", bins = y_bin) +
  geom_density(aes(y = ..count..)) +
  facet_wrap(~time.bin) +
  theme_classic() +
  labs(title = "Mobile Fraction")
if (missing(overlay)) {
  p3 <- p1 + p2
} else {
  message("plotting residence density by time")
  p4 <- df %>%
    dplyr::filter(state != "pause") %>%
    ggplot(aes(x = y_numeric)) +
    # geom_histogram(fill = "grey", bins = y_bin, alpha = 0.2, aes(colour = time.bin)) +
    geom_density(aes(y = ..count.., fill = dens.time.bin), alpha =.7) +
    theme_classic() +
    labs(title = "Mobile Fraction", fill = "time bin") +
    #viridis::scale_color_viridis(discrete = TRUE) +
    viridis::scale_fill_viridis(discrete = TRUE)
  p3 <- p4 + { p1 + p2 + plot_layout(nrow = 1) } + plot_layout(ncol=1)
}
p3
message("writing pdf to disk")
ggsave(file.path(folder, paste0(file.pref[1], "residency.pdf")), plot = p3, device = "pdf", width = 8, height = 6)
  }

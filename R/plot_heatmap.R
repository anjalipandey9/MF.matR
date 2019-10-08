#' plot_heatmap
#' Generate a heatmap from a previously analyzed GCaMP dataset using plotGCaMP_multi - output is ggplot object which can be modified further
#' @param heatmap_limits optional 3-value vector defining the color scale limits, ie c(-1,0,2)
#' @param response_time time of expected GCaMP response in seconds. heatmaps will be arranged in descending amplitude based on these responses.
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%$%"
#' @export
#' @examples plot <- plot_heatmap()
#'

plot_heatmap <- function(heatmap_limits = "auto",
                         response_time = 59.5,
                         ...) {
  library(tidyverse)
  library(scales)
  data <- read_csv(file.choose()) %>%
    mutate(animal_num = as.factor(animal_num))
  # full_join(data, plot_order) %>%
  # unnest() %>%

  if(!is.numeric(heatmap_limits)) { # using auto calc unless a numeric vector input
    breaks <- round(
      data %>% unnest %$% quantile(delF, c(0.05, 0.5, 0.99)),
      2
    )
    labels <- as.character(breaks)
    limits <- breaks[c(1,3)]
  } else {
    breaks <- heatmap_limits
    labels <- as.character(breaks)
    limits <- breaks[c(1,3)]
  }

  labels <- as.character(breaks)
  limits <- breaks[c(1,3)]

  plot_order <- data %>%
    group_by(animal, animal_num) %>%
    summarise(maxD = MF.matR::max_delta(delF, end = response_time)) %>%
    arrange(maxD)

  full_join(data, plot_order, cols = c("animal", "animal_num", "maxD")) %>% group_by(animal_num) %>%
  ggplot(aes(x = time, y = fct_reorder(animal_num, maxD))) +
  geom_tile(aes(fill = signal)) +
  scale_fill_viridis_c(option = "magma",
                       breaks = breaks,
                       labels = labels,
                       limits = limits,
                       oob =squish) +
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.text.y = element_blank()) +
  labs(y = "Animal number") }

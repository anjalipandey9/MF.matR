#' plotAvgResidency
#'
#' Plots average residency for stripe data
#' @param FileFilter string to search/subset filenames
#'
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%$%"
#' @export
#' @examples data <- plotAvgResidency()

plotAvgResidency <- function(folderPath,
                                  y_bins = 50,
                                  y_max = 5,
                                  ...) {

  message("select a file in the folder you want to analyze - you need a file in the folder to select")
  library(tidyverse)
  library(scales)
  library(fs)
  library(data.table)
  ggplot2::theme_set(theme_classic())

  #### making an interactive option for the base folder:
  if(missing(folderPath)) {
    folderPath <- dirname(file.choose())
  }
  message(paste("using files at or below the folder:", basename(folderPath)))

  merged_df <- folderPath %>%
    fs::dir_ls(path = .,
               glob = "*raw_residence.csv*",
               recurse = TRUE) %>%
    map_df(~data.table::fread(.),.id = "filename") %>%
    tibble() %>%
    mutate(filename = factor(basename(filename)))

  #calculate relateive residence per assay
  #
  # rel_merged_df <- merged_df %>%
  #   mutate(ybin = cut(y_mm, 100)) %>%
  #   group_by(filename,ybin) %>%
  #   tally() %>%
  #   separate(ybin, into = c("spacer", "lower", "upper"), sep = "[(,\\]]") %>%
  #   mutate(upper = as.numeric(upper),
  #          lower = as.numeric(lower),
  #          y_mm = (lower + upper) / 2)
  #
  # total_n_assay <- rel_merged_df %>%
  #   group_by(filename) %>%
  #   summarize(mean_count = mean(n))
  #
  # plot <- full_join(rel_merged_df, total_n_assay, by = "filename") %>%
  #   mutate(rel.res = n/mean_count) %>%
  #   group_by(y_mm) %>%
  #   summarize(mean_relRes = mean(rel.res)) %>%
  #   ggplot(aes(x = y_mm)) +
  #   geom_histogram(stat = "identity",
  #                  aes(y = mean_relRes),
  #                  width = rel_merged_df$upper[1]-rel_merged_df$upper[2]) +
  #   labs(x = "position (mm)",
  #        y = "relative residence") +
  #   scale_x_continuous(breaks = c(0,5,10,15)) +
  #   scale_y_continuous(limits = c(0, y_max))
  #
  # ggsave(plot = plot, filename = file.path(folderPath,paste0(basename(folderPath),"_averageHistogram.pdf")), width = 4, height = 4, units = "in")


}

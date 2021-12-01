#' plotAvgResidency
#'
#' Plots average residency for stripe data
#' if you have folders with "rel_residence.csv" files in them, this will aggregate
#' and plot the average across multiple experiments
#' @param FileFilter string to search/subset filenames
#' @param y_bins the number of bins for the plot
#' @param y_max maximum bar height to plot in relative residence
#' @param time_bins if previous analysis included a time binning,
#' you can include that argument here to plot multiple time bins
#'
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%$%"
#' @export
#' @examples data <- plotAvgResidency()

plotAvgResidency <- function(folderPath,
                                  y_bins = 50,
                                  y_max = 4,
                                  fillcolor = "grey",
                                  bordercolor = NA,
                             time_bins = 1,
                                  ...) {

  if(fillcolor == "grey")
    message("you've opted to go with the default color scheme - to change this enter the optional arguments `fillcolor` and `bordercolor`")

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
               glob = "*rel_residence.csv*",
               recurse = TRUE) %>%
    map_df(~data.table::fread(.),.id = "filename") %>%
    tibble() %>%
    mutate(filename = factor(basename(filename)))



  if(time_bins == 1) {
  plot <- merged_df %>%
    group_by(ybin_numeric) %>%
    summarize(relres = mean(relres)) %>%
    ggplot() +
    geom_rect(aes(xmin = 0,
                  xmax = relres,
                  ymin = (16.2/50)*(ybin_numeric-.5),
                  ymax = (16.2/50)*(ybin_numeric+.5)),
                  fill = fillcolor,
                  color = bordercolor) +
    labs(y = "position (mm)",x = "relative residence") +
    scale_x_continuous(limits = c(0,y_max), expand = c(0,0))
  } else {
    plot <- merged_df %>%
      drop_na(relres) %>%
      group_by(ybin_numeric, time_bin) %>%
      summarize(relres = mean(relres)) %>%
      ggplot() +
      geom_rect(aes(xmin = 0,
                    xmax = relres,
                    ymin = (16.2/50)*(ybin_numeric-.5),
                    ymax = (16.2/50)*(ybin_numeric+.5)),
                fill = fillcolor,
                color = bordercolor) +
      labs(y = "position (mm)",x = "relative residence") +
      scale_x_continuous(limits = c(0,y_max), expand = c(0,0)) +
      facet_grid(.~time_bin)
  }


  ggsave(plot = plot, filename = file.path(folderPath,paste0(basename(folderPath),"_averageHistogram.pdf")), width = 4, height = 4, units = "in")

  return(list(merged_df,plot))
}

#' plotGCaMP_multi
#'
#' This function replicates prism-like scatter plots with a bar-graph and a scatterplot with SEM values.
#' It requires discrete x, but can use interaction of multiple values to set x. Default is to colour value #1 black, and all else blue.
#' output is a ggplot object that can be further modified.
#'
#' @param data input dataset
#' @param xval main x-value (can be interaction of two factors)
#' @param yval y-values (continuous data)
#' @param facet factor to facet data
#' @param color factor to set colors
#' @param shape factor to set shape of scatterplot
#' @param ncolor number of colors to include (must match )
#' @export
#' @examples p <- Prism_scatter(data, x = interaction(treatment,stage), y = odr10GFP, color = Dev_experience, shape = treatment, facet = Dev_experience, ncolors = 2)

Prism_scatter <- function(data, xval, yval, facet, color_var, shape, ncolors, nbreaks, ...) {
  library(tidyverse)
  xval <- enquo(xval)
  yval <- enquo(yval)
  facet <- enquo(facet)
  color_var <- enquo(color_var)
  shape <- enquo(shape)

  ncolors <- data %>% select(!!color_var) %>% unique() %>% nrow()
  print(ncolors)

  ymax <- data %>% select(!!yval) %>% max()

  data %>% ggplot(aes(x = !!xval, y = !!yval)) + #place x-axis values in order you want (divide by control and dauer entry with facet)
    stat_summary(fun.y=mean, #add the bars
                 geom="bar", color="grey", width=0.8, fill = "white") +
    geom_jitter(aes(colour = !!color_var, shape = !!shape), width = 0.2, alpha = 0.75) + #add jittered points, make them a little transparent aes_string(shape = shape, colour = !!color)
    facet_wrap(vars(!!facet), scales = "free_x") + #scales "free' will drop missing levels of the x-axis (ie dauer.starved)
    theme_classic() + #make background white
    scale_color_manual(values = c("black", rep("blue",(ncolors-1)))) +
    stat_summary(fun.data=mean_se, fun.args = list(mult=1), # add error bars
                 geom="errorbar", aes(colour=!!color_var, width=0.4)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0,ymax+0.1*ymax)) + #manually set the axes (careful scale_y can eliminate values not in range)
    labs(x = "", y = "GFP Fluorescence (AU)") +
    theme(axis.text.x = element_blank(), #remove x-axis labels +
          strip.text.x = element_blank(), #remove facet labels +
          axis.text.y = element_text(size = 15), #make text a little bigger
          axis.title.y =  element_text(size = 15)) + #make text a little bigger
    guides(colour = FALSE, shape = FALSE) # get rid of legend
}

#' Plot state probability from Wormlab data by space and time.
#' @param df data frame
#' @param time_bin number of bins to split plot
#' @param y_bin number of position bins
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
#' @examples
#' plot_stateProbs <-
#'

plot_stateProbs <- function(time_bin, y_bin) {
  message("choose all track data .csv file")
  file <- file.choose()
  folder <- dirname(file)
  file.pref <- basename(file) %>% strsplit(., "all_track_data.csv") %>% unlist()
  #devtools::install_github("thomasp85/patchwork")
  library(ggplot2)
  library(patchwork)
  message("reading in data")
  df <- read.csv(file)

    alldata.enter<-MF.matR::norm_beh_enter(df)
    alldata.exit<-MF.matR::norm_beh_exit(df)

    plot1 <- alldata.enter %>% dplyr::ungroup() %>%
      dplyr::mutate(time.bin = dplyr::ntile(Time,time_bin),
           y.bin = dplyr::ntile(norm.y,y_bin),
           state = factor(state)) %>%
    dplyr::count(time.bin,y.bin,state) %>%
    dplyr::group_by(time.bin,y.bin) %>% #maybe group by worm as well
    dplyr::mutate(prop = prop.table(n)) %>%
    ggplot(aes(x = y.bin, y = prop)) +
      annotate("rect", xmin=50,xmax=100,ymin=0,ymax =1, fill="lightblue", alpha=0.3) +
    geom_point(aes(color = state), alpha = 0.2) + geom_smooth(aes(group = state, colour = state)) +
    facet_wrap(~time.bin) + labs(title = "Stripe Entry") + guides(colour = FALSE)

    plot2 <- alldata.exit %>% dplyr::ungroup() %>%
    dplyr::mutate(time.bin = dplyr::ntile(Time,time_bin),
                  y.bin = dplyr::ntile(y,y_bin),
                  state = factor(state)) %>%
    dplyr::count(time.bin,y.bin,state) %>%
    dplyr::group_by(time.bin,y.bin) %>% #maybe group by worm as well
    dplyr::mutate(prop = prop.table(n)) %>%
    ggplot(aes(x = y.bin, y = prop)) +
      annotate("rect", xmin=0,xmax=50,ymin=0,ymax =1,fill="lightblue", alpha=0.3) +
    geom_point(aes(color = state), alpha = 0.2) + geom_smooth(aes(group = state, colour = state)) +
    facet_wrap(~time.bin) + labs(title = "Stripe Exit")

    plot3 <- alldata.enter %>% dplyr::ungroup() %>%
      dplyr::filter(state != "pause") %>%
      dplyr::mutate(time.bin = dplyr::ntile(Time,time_bin),
                    y.bin = dplyr::ntile(norm.y,y_bin),
                    state = factor(state)) %>%
      dplyr::count(time.bin,y.bin,state) %>%
      dplyr::group_by(time.bin,y.bin) %>% #maybe group by worm as well
      dplyr::mutate(prop = prop.table(n)) %>%
      ggplot(aes(x = y.bin, y = prop)) +
      annotate("rect", xmin=50,xmax=100,ymin=0,ymax =1, fill="lightblue", alpha=0.3) +
      geom_point(aes(color = state), alpha = 0.2) + geom_smooth(aes(group = state, colour = state)) +
      facet_wrap(~time.bin) + labs(title = "Stripe Entry (mobile fraction)") + guides(colour = FALSE)

    plot4 <- alldata.exit %>% dplyr::ungroup() %>%
      dplyr::filter(state != "pause") %>%
      dplyr::mutate(time.bin = dplyr::ntile(Time,time_bin),
                    y.bin = dplyr::ntile(y,y_bin),
                    state = factor(state)) %>%
      dplyr::count(time.bin,y.bin,state) %>%
      dplyr::group_by(time.bin,y.bin) %>% #maybe group by worm as well
      dplyr::mutate(prop = prop.table(n)) %>%
      ggplot(aes(x = y.bin, y = prop)) +
      annotate("rect", xmin=0,xmax=50,ymin=0,ymax =1,fill="lightblue", alpha=0.3) +
      geom_point(aes(color = state), alpha = 0.2) + geom_smooth(aes(group = state, colour = state)) +
      facet_wrap(~time.bin) + labs(title = "Stripe Exit (mobile fraction)")

    plot5 <- alldata.enter %>% dplyr::ungroup() %>%
      dplyr::filter(state != "pause") %>%
      dplyr::mutate(time.bin = dplyr::ntile(Time,time_bin),
                    y.bin = dplyr::ntile(norm.y,y_bin),
                    state = factor(state)) %>%
      dplyr::count(time.bin,y.bin,state) %>%
      dplyr::group_by(time.bin,y.bin) %>% #maybe group by worm as well
      dplyr::mutate(prop = prop.table(n)) %>%
      dplyr::filter(state == "reversal") %>%
      ggplot(aes(x = y.bin, y = prop)) +
      annotate("rect", xmin=50,xmax=100,ymin=0,ymax =0.2, fill="lightblue", alpha=0.3) +
      geom_point(aes(color = state), alpha = 0.2) + geom_smooth(aes(group = state, colour = state)) +
      facet_wrap(~time.bin) + labs(title = "Stripe Entry (mobile fraction)") + guides(colour = FALSE)

    plot6 <- alldata.exit %>% dplyr::ungroup() %>%
      dplyr::filter(state != "pause") %>%
      dplyr::mutate(time.bin = dplyr::ntile(Time,time_bin),
                    y.bin = dplyr::ntile(y,y_bin),
                    state = factor(state)) %>%
      dplyr::count(time.bin,y.bin,state) %>%
      dplyr::group_by(time.bin,y.bin) %>% #maybe group by worm as well
      dplyr::mutate(prop = prop.table(n)) %>%
      dplyr::filter(state == "reversal") %>%
      ggplot(aes(x = y.bin, y = prop)) +
      annotate("rect", xmin=0,xmax=50,ymin=0,ymax = 0.2,fill="lightblue", alpha=0.3) +
      geom_point(aes(color = state), alpha = 0.2) + geom_smooth(aes(group = state, colour = state)) +
      facet_wrap(~time.bin) + labs(title = "Stripe Exit (mobile fraction)")

  plot_all <- (plot1 + plot2 &
  scale_colour_manual(values = c("#8A2BE2", "#0000FF", "#7FFF00", "#006400", "#FF7256"))) /
  (plot3 + plot4 &
    scale_colour_manual(values = c("#8A2BE2", "#0000FF", "#7FFF00", "#FF7256"))) /
  (plot5 + plot6 &
    scale_colour_manual(values = "#FF7256")) &
  theme_classic()

  ggsave(file.path(folder, paste0(file.pref[1], "state_probs.pdf")), plot = plot_all, device = "pdf", width = 8, height = 10)
}


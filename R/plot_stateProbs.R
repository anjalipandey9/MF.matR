#' Plot state probability from Wormlab data by space and time.
#' @param df data frame
#' @param time_bin number of bins to split plot
#' @param y_bin number of position bins
#' @param orient orientation of the cue, 1 = center stripe, 0 = outer 2 stripes
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
#' @examples
#' plot_stateProbs(time_bin = 4, y_bin  = 100, orient = 1)

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

  if(orient == 1) {
    message("setting orientation with cue in middle stripe")
    alldata.enter<-MF.matR::norm_beh_enter(df)
    alldata.exit<-MF.matR::norm_beh_exit(df)
  }

  if(orient == 0) {
    message("setting orientation with cue in outer stripes")
    alldata.enter<-MF.matR::norm_beh_exit(df)
    alldata.exit<-MF.matR::norm_beh_exit(df)
  }

    probs_all <- . %>% dplyr::ungroup() %>%
      dplyr::mutate(time.bin = as.numeric(cut(Time,time_bin)),
                    y.bin = as.numeric(cut(norm.y,y_bin)),
                    state = factor(state)) %>%
      dplyr::count(time.bin,y.bin,state) %>%
      dplyr::group_by(time.bin,y.bin) %>% #maybe group by worm as well
      dplyr::mutate(prop = prop.table(n)) %>%
      group_by(time.bin, y.bin) %>% filter(sum(n) > 10)

    probs_mobile <- . %>% dplyr::ungroup() %>%
      dplyr::filter(state != "pause") %>%
      dplyr::mutate(time.bin = as.numeric(cut(Time,time_bin)),
                    y.bin = as.numeric(cut(norm.y,y_bin)),
                    state = factor(state)) %>%
      dplyr::count(time.bin,y.bin,state) %>%
      dplyr::group_by(time.bin,y.bin) %>% #maybe group by worm as well
      dplyr::mutate(prop = prop.table(n)) %>%
      group_by(time.bin, y.bin) %>% filter(sum(n) > 10)


    plot1 <- alldata.enter %>% probs_all() %>%
    ggplot(aes(x = y.bin, y = prop)) +
      annotate("rect", xmin=50,xmax=100,ymin=0,ymax =1, fill="lightblue", alpha=0.3) +
    geom_point(aes(color = state, alpha = n)) + geom_smooth(aes(group = state, colour = state, weight = n)) +
    facet_wrap(~time.bin, scales = "free") + labs(title = "Stripe Entry") + guides(colour = FALSE)

    plot2 <- alldata.exit %>% probs_all() %>%
    ggplot(aes(x = y.bin, y = prop)) +
      annotate("rect", xmin=0,xmax=50,ymin=0,ymax =1,fill="lightblue", alpha=0.3) +
    geom_point(aes(color = state, alpha = n)) + geom_smooth(aes(group = state, colour = state,weight = n)) +
    facet_wrap(~time.bin, scales = "free") + labs(title = "Stripe Exit")

    plot3 <- alldata.enter %>% probs_mobile() %>%
      filter(state != "pause") %>%
      ggplot(aes(x = y.bin, y = prop)) +
      annotate("rect", xmin=50,xmax=100,ymin=0,ymax =1, fill="lightblue", alpha=0.3) +
      geom_point(aes(color = state, alpha = n)) + geom_smooth(aes(group = state, colour = state, weight = n)) +
      facet_wrap(~time.bin, scales = "free") + labs(title = "Stripe Entry (mobile fraction)") + guides(colour = FALSE)

    plot4 <- alldata.exit %>% probs_mobile() %>%
      filter(state != "pause") %>%
      ggplot(aes(x = y.bin, y = prop)) +
      annotate("rect", xmin=0,xmax=50,ymin=0,ymax =1,fill="lightblue", alpha=0.3) +
      geom_point(aes(color = state, alpha = n)) + geom_smooth(aes(group = state, colour = state, weight = n)) +
      facet_wrap(~time.bin, scales = "free") + labs(title = "Stripe Exit (mobile fraction)")

    plot5 <- alldata.enter %>% probs_mobile() %>%
      dplyr::filter(state == "reversal") %>%
      ggplot(aes(x = y.bin, y = prop)) +
      annotate("rect", xmin=50,xmax=100,ymin=0,ymax =0.2, fill="lightblue", alpha=0.3) +
      geom_point(aes(color = state, alpha = n)) + geom_smooth(aes(group = state, colour = state, weight = n)) +
      facet_wrap(~time.bin, scales = "free") + labs(title = "Stripe Entry (mobile fraction)") + guides(colour = FALSE)

    plot6 <- alldata.exit %>% probs_mobile() %>%
      dplyr::filter(state == "reversal") %>%
      ggplot(aes(x = y.bin, y = prop, weight = n)) +
      annotate("rect", xmin=0,xmax=50,ymin=0,ymax = 0.2,fill="lightblue", alpha=0.3) +
      geom_point(aes(color = state, alpha = n)) + geom_smooth(aes(group = state, colour = state)) +
      facet_wrap(~time.bin, scales = "free") + labs(title = "Stripe Exit (mobile fraction)")

  plot_all <- (plot1 + plot2 &
  scale_colour_manual(values = c("#8A2BE2", "#0000FF", "#7FFF00", "#006400", "#FF7256"))) /
  (plot3 + plot4 &
    scale_colour_manual(values = c("#8A2BE2", "#0000FF", "#7FFF00", "#FF7256"))) /
  (plot5 + plot6 &
    scale_colour_manual(values = "#FF7256")) &
  theme_classic()

  ggsave(file.path(folder, paste0(file.pref[1], "state_probs.pdf")), plot = plot_all, device = "pdf", width = 8, height = 10)
}


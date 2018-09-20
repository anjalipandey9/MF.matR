#' plotGCaMP_multi
#'
#' Function is a wrapper for exp.fit.all.log.lin which outputs corrected GCaMP signals as well as plots showing
#' original and corrected signals. Also plots average trace for GCaMP signal. Inputs are matfiles.
#' The script searches recursively for matfiles from a startPulseer file (can be any placeholder file). Need to exclude
#' matfiles that are not GCaMP files either using FileFilter, or by putting only relevant files in the folder. Default
#' startpulse assumes 400ms delay for camera recording.
#' Requires max_delta helper function
#' time etc...
#' @param FileFilter string to search/subset filenames
#' @param matlab whether to use matfiles or data from ImageJ quantification. Defaults to TRUE (matfiles)
#' @param genotype label the genotype for these data
#' @param cue label the stimulus cue.
#' @param food label to food cue
#' @param startPulse begin of stimulus
#' @param endPulse endPulse time of stimulus
#' @param center_on_pulse optional parameter to center delF values by the mean of the stimulus duration
#' 1 = Bring values to mean delF of 2nd half pulse duration, order heatmaps by OFF responses (magnitude of increase)
#' 2 = Bring values to mean delF of 2nd half pre-pulse duration, order by ON responses (magnitude of increase)
#' 3 = Bring values to mean delF of 2nd half pulse duration, order heatmaps by OFF responses (magnitude of decrease)
#' 4 = Bring values to mean delF of 2nd half pre-pulse duration, order by ON responses (magnitude of decrease)
#' 'OFF' = Bring values to mean delF of 2nd half pre-pulse duration, order by OFF responses
#' 'ON' = Bring values to mean delF of 2nd half pre-pulse duration, order by ON responses
#' @param show.plots render plots for baseline correction - defaults to TRUE
#' @param use.Fmax normalize amplitude to 1
#' @param neuron neuron being analyzed
#' @param linear optional argument piped into exp.fit.all.log.lin include a linear term in the fit?
#'
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @export
#' @examples data <- plotGCaMP_multi(N2, genotype = N2, cue = octanol)
#'
plotGCaMP_multi <- function(FileFilter,
                            matlab = TRUE,
                            genotype,
                            cue = cue,
                            food = OP50,
                            startPulse = 29.5,
                            endPulse = 59.5,
                            center_on_pulse = FALSE,
                            show.plots = TRUE,
                            use.Fmax = FALSE,
                            neuron = GCAMP,
                            linear = FALSE,
                            nls = TRUE,
                            ...) {
  library(tidyverse)
  library(magrittr)
  library(patchwork)
  library(scales)
  FileFilter <- quo_name(enquo(FileFilter)) # make Filter usable inside other functions
  genotype <- quo_name(enquo(genotype))
  cue <- quo_name(enquo(cue))
  food <- quo_name(enquo(food))
  neuron <- quo_name(enquo(neuron))
  center_on_pulse <- quo_name(enquo(center_on_pulse))

  folderPath <- dirname(file.choose())
  print(folderPath)

  if(matlab == TRUE) {
    files <- list.files(file.path(folderPath), pattern = "*.mat", recursive = TRUE)
    files <- files[stringr::str_detect(files, pattern = paste0(FileFilter))]
    filenames <- files
    files <- file.path(folderPath, files)
    #df <- data.frame(x = 1, genotype = genotype, cue = cue)

    data <- map(files, ~ exp.fit.all.log.lin(filename = .,
                                             skip.time = 10,
                                             show.plots = show.plots,
                                             nls = nls,
                                             startPulse = startPulse,
                                             endPulse = ))
                } else {
                  neuronfiles <- list.files(file.path(folderPath), pattern = "*neuron_results.csv", recursive = TRUE)
                  neuronfiles <- neuronfiles[stringr::str_detect(neuronfiles, pattern = paste0(FileFilter))]
                  neuronfilenames <- neuronfiles

                  neuronfiles <- file.path(folderPath, neuronfiles)
# backgroundfiles <- list.files(file.path(folderPath), pattern = "*background_results.csv", recursive = TRUE)
# backgroundfiles <- neuronfiles[stringr::str_detect(backgroundfiles, pattern = paste0(FileFilter))]
# backgroundfiles <- file.path(folderPath, backgroundfiles)

                  purrr::map(
                    neuronfiles,
                    ~ merge_FIJI_data(neuronfile = ., show.plots = show.plots))

                  files <- list.files(file.path(folderPath), pattern = "*ImageJ_data.csv", recursive = TRUE)
                  files <- files[stringr::str_detect(files, pattern = paste0(FileFilter))]
                  filenames <- files
                  files <- file.path(folderPath, files)
                  data <- map(files, ~ exp.fit.all.log.lin(
                    filename = .,
                    skip.time = 10,
                    show.plots = show.plots,
                    matlab = FALSE,
                    linear = linear,
                    nls = nls
))
  }


  data %<>% data_frame(data = .,
                       animal = filenames,
                       animal_num = factor(seq(from = 1, to = length(filenames))),
                       genotype = genotype,
                       cue = cue,
                       food = food,
                       neuron = neuron)


  # recenter mean values
  if(center_on_pulse == "OFF") {

    means <- data %>% unnest %>%
      group_by(animal) %>%
      filter(time > (startPulse+endPulse)/2 & time < endPulse) %>%
      summarise(mean_pulse_delF = mean(delF))

    data <- full_join(data, means)

    data %<>% unnest %>%
      group_by(animal, animal_num) %>%
      mutate(delF = delF - mean_pulse_delF) %>%
      nest()
  }

  if(center_on_pulse == "ON") {

    means <- data %>% unnest %>%
      group_by(animal) %>%
      filter(time > startPulse/2 & time < startPulse) %>%
      summarise(mean_pulse_delF = mean(delF))

    data <- full_join(data, means)

    data %<>% unnest %>%
      group_by(animal,animal_num) %>%
      mutate(delF = delF - mean_pulse_delF) %>%
      nest()
  }

  # arrange heat map settings
  if(center_on_pulse == "OFF") {
    plot_order <- data %>%
      unnest() %>%
      group_by(animal, animal_num) %>%
      summarise(maxD = max_delta(delF, end = endPulse)) %>%
      arrange(maxD)

    breaks = c(-.5,0,1.5)
    labels = c("-.5", "0", "1.5")
    limits = c(-0.5,1.5)
  }

  if(center_on_pulse %in% c("ON", FALSE)) {
    plot_order <- data %>%
      unnest() %>%
      group_by(animal, animal_num) %>%
      summarise(maxD = max_delta(delF, end = startPulse)) %>%
      arrange(maxD)

    breaks = c(-.5,0,1.5)
    labels = c("-.5", "0", "1.5")
    limits = c(-0.5,1.5)
  }

  if(use.Fmax == TRUE) {
    range <- data %>% unnest %>%
      group_by(animal) %>%
      summarise(max_delF = max(delF), Fo = quantile(delF, 0.05))

    data <- full_join(data, range)

    data %<>% unnest %>%
      group_by(animal,animal_num) %>%
      mutate(delF = (delF - Fo) / (max_delF - Fo)) %>%
      nest()

    breaks = c(0,0.5,1)
    labels = c("0", "0.5", "1")
    limits = c(0,1)

  }


  plot1 <- data %>% unnest() %>%
    ggplot(aes(x = time, y = delF)) +
    geom_line(aes(group = animal), alpha = 0.1) +
    geom_smooth(method = "loess", span = 0.05) +
    theme_classic() +
    geom_segment(aes(x = !!startPulse,
                     y = limits[2],
                     xend = !!endPulse,
                     yend = limits[2])) +
    annotate(geom = "text",
             label = cue,
             y = limits[2] + 0.2,
             x = (startPulse + endPulse)/2,
             size = 8) +
    annotate(geom = "text",
             label = genotype,
             y = limits[2] + 0.2,
             x = 10,
             fontface = "italic",
             size = 8) +
    coord_cartesian(ylim = c(limits[1], limits[2]+0.5)) +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 18),
          axis.title.x = element_blank())

  plot2 <-  full_join(data, plot_order) %>%
    unnest() %>%
    group_by(animal_num) %>%
    ggplot(aes(x = time, y = fct_reorder(animal_num, maxD))) +
    geom_tile(aes(fill = delF)) +
    scale_fill_viridis_c(option = "magma",
                         breaks = breaks,
                         labels = labels,
                         limits = limits,
                         oob =squish) +
    theme_classic() +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 18),
          axis.text.y = element_blank()) +
    labs(y = "Animal number")

  plots <- plot1 + plot2 + plot_layout(ncol = 1, heights = c(2,1))

  if(use.Fmax == TRUE) {
    ggsave(plots, filename = file.path(folderPath,paste0(genotype,"_",cue,neuron,"_delFmaxplots.png")),
           width = 11, height = 8.5, units = "in")
  } else {
    ggsave(plots, filename = file.path(folderPath,paste0(genotype,"_",cue,neuron,"_plots.png")),
           width = 11, height = 8.5, units = "in")
  }

  return(list(data = dplyr::full_join(data, plot_order), plot = plots))
}


#' merge_FIJI_data
#'
#' This helper function takes a group of measurements output from registerquantGCaMP.ijm imageJ macro
#' and generates a merged .csv file with mean neuron pixel values, mean background pixel values, and the
#' background-subtracted pixel values - ie the 'signal'. No bleaching correction is applied. Files are written
#' in place to the enclosing folder.
#'
#' @param filename
#'
merge_FIJI_data <- function(neuronfile, backgroundfile) {
  neuron <- read.csv(neuronfile) %>% dplyr::select(animal = Label, MeanGCaMP = Mean, Slice)
  background <- read.csv(backgroundfile) %>% dplyr::select(animal = Label, MeanBackground = Mean, Slice)

  data <- dplyr::full_join(neuron, background) %>%
    separate(animal, "animal", sep = ":") %>%
    mutate(Fluor = MeanGCaMP - MeanBackground,
           Fnaut = mean(Fluor[1:20]),
           signal = (Fluor - Fnaut) / Fnaut)
}

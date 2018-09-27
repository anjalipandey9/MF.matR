#' getAmpVdelF
#'
#' @param FileFilter string to search/subset filenames

#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @export
#' @examples data <- getAmpVdelF(FileFilter = N2)
#'
getAmpVdelF <- function(FileFilter,...) {
  FileFilter <- quo_name(enquo(FileFilter))
  folderPath <- dirname(file.choose())
  files <- list.files(file.path(folderPath), pattern = "*ImageJ_data.csv", recursive = TRUE)
  files <- files[stringr::str_detect(files, pattern = paste0(FileFilter))]
  filenames <- files
  files <- file.path(folderPath, files)

  data <- map(files, function(csvfile) {
    read_csv(csvfile) %>%
      mutate(filefolder = basename(dirname(csvfile)),
             animal = interaction(filefolder,animal),
             basefolder = basename(dirname(dirname(csvfile))))}) %>%
    bind_rows() %>%
    group_by(animal,filefolder,basefolder) %>%
    summarise(startGCaMP = mean(MeanGCaMP[1:20]),
              relGCaMP = mean(MeanGCaMP[1:20]) - mean(MeanBackground[1:20]),
              background = mean(MeanBackground[1:20]),
              bleach = mean(MeanGCaMP[98:120]) - mean(MeanGCaMP[1:20]),
              maxD = max_delta(signal, end = 30))

  p <- ggplot(data, aes(x = startGCaMP, y = maxD)) +
    geom_point(aes(colour = basefolder)) + geom_smooth()

  print(p)

  return(data)
}


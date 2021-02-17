#' plot_speedFood
#' plot roaming speed across food conditions
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
#' @examples
#' plot_speedFood()


plot_speedFood <- function(...) {
  library(tidyverse)
  library(patchwork)
  message("click on a file in the base folder")
  folder <- dirname(file.choose())
  files <- fs::dir_ls(folder, glob = "*track_data.csv", recurse = TRUE)
  speedData <- files %>% map_df(read_csv, .id = "file.path") %>%
    select(file.path, Time, bin.speed.man, time.bin, bin.ang.vel, worm)

  newdata <- speedData %>%
    filter(bin.speed.man/bin.ang.vel > 2) %>%
    mutate(plate = basename(dirname(file.path)),
           worm = interaction(worm, plate)) %>%
    group_by(plate) %>%
    nest() %>%
    separate(plate, into = c("genotype", "cultFood", "AssayCond", "plateNum"))
  message("here's your experimental conditions")
  newdata

  # explore all worm #s
  p1 <- newdata %>% unnest(cols = c(data)) %>%
    ggplot(aes(x = cultFood, y = bin.speed.man)) +
    geom_boxplot(aes(fill = cultFood)) +
    facet_grid(.~AssayCond) +
    guides(fill = FALSE)

  # explore all worm #s
  p2 <- newdata %>% unnest(cols = c(data)) %>%
    ggplot(aes(x = plateNum, y = bin.speed.man)) +
    geom_boxplot(aes(fill = cultFood)) +
    facet_grid(AssayCond~cultFood) +
    guides(fill = FALSE)

  p1 + p2

}

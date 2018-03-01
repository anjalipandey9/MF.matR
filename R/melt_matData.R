#' melt_matData
#' fucntion to tidy up matlab datasets which are in long format and separated by assay
#' @param df input data list, needs to be from readMat input, with naemed conditions
#' @param datatype type of data you are reorganizing (ie state, pulse # etc..)
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
#' @examples
#' melt_matData(states, datatype = state)

melt_matData <- function(df, datatype) {
  datatype <- enquo(datatype)
  columnName <- quo_name(datatype)
  require(dplyr)
  #column = quo(datatype)
  reshape2::melt(df) %>%
    dplyr::rename(worm = Var1,
                  video.frame = Var2,
                  !!columnName := value,
                  cue = L1,
                  assay = L2) %>%
    mutate(cue = rep(conditions, rle(.$cue)$lengths),
           assay = rep(as.character(unlist(filenames)), rle(.$assay)$lengths))
}

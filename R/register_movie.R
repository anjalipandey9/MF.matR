#' register_movie
#'
#' simple R function to run an imageJ registration macro within R. Can be used to loop
#' through a list of video files.
#'
#' @export

register_movie <- function() {

  if (file.exists('/Applications/Fiji.app')) {
    ImageJ_path <- '/Applications/Fiji.app/Contents/MacOS/ImageJ-macosx'

    system2(ImageJ_path,
            args = c("-macro", file.path(.libPaths(),
                                         'MF.matR/register_quantGCaMP.ijm')))
  } else {
    message("no ImageJ install package found in /Applications, please install Fiji.app.")
  }

}

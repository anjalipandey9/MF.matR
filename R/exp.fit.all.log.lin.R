#' exp.fit.all.log.lin
#'
#' Function takes a list of calcium imaging files by filenames in the current working directory (.mat files)
#' and performs a simple exponential regression fit to normalize the deltaF/F signal. This function incorporates a linear
#' to account for gradual rise in signal, which can alter the single exponential. Will output plots showing orignal
#' and fitted values, as well as corrected values. If an object is assigned, this will be a vector of corrected values
#' @param filename filepaths of .mat files which have a "signal"  and "time" field.
#' @param skip.time number of seconds to skip at the beginning for the exponential fit. N=10 improves the fit.
#' @param nls use nls to perform exponential fit: signal ~ y0 + exp((-time - T0) / tau)
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @export
#' @examples data <- exp.fit.all.log.lin(files[1], skip.time = 10)
#'
exp.fit.all.log.lin <- function(filename,
                                skip.time,
                                matlab = TRUE,
                                show.plots = TRUE,
                                linear = TRUE,
                                nls = FALSE,
                                startPulse = 29.5,
                                endPulse = 60.5) {
  if(matlab == TRUE) {
    matfile <- R.matlab::readMat(filename, fixNames = TRUE)
    signal <- matfile$signal
    time <- (1:length(signal)) / 4
    df <- data.frame(time, signal)
    rm(signal)
    rm(time)
  } else {
    df <- read_csv(filename) %>% dplyr::select(signal, MeanGCaMP, time)
  }


  animal_name <- basename(filename)
    #quo_name(enquo(filename))

  # fit to first N(skip.time) seconds to 30 sec for log-linear fit
  # if using nls, fit only to pre and post-stimulus

  if(nls == FALSE) {
    if(linear == FALSE) {
      fit1 <- lm(data = df[c(skip.time:120, 300:360), ], signal ~ log(time))
      correction <- "log"
    } else {
      fit1 <- lm(data = df[c(skip.time:120, 300:360), ], signal ~ log(time) + time) # plus last 15s
      correction <- "log+linear"
      if(fit1$coefficients[2] > 0) {
        fit1 <- lm(data = df[c(skip.time:120, 300:360), ], signal ~ log(time))
        correction <- "log"
      }
    }

  } else { #for nls = FALSE
      fit1 <- try(nls(signal ~ SSasymp(time, Asym, R0, lrc),
                  data = dplyr::filter(df, time < 29 | time > (60 + 10))))
      correction <- "nls"
  }

  #### get fitted values ####
if (inherits(fit1, "try-error")) {
  message("No exponential decay detected, using raw values for file:")
  print(filename)
  fitted <- 0
  correction <- "raw"
} else {
  fitted <- predict(fit1, newdata = df)
}



 df %<>% mutate(fitted = fitted,
                correction = correction)

  # for linear fit,
  # correct after fitted values go below zero (~ 20s)
  if(nls == FALSE) {
    df %<>% mutate(corrected = dplyr::case_when(
      fit1$coefficients[2] > 0 ~ signal, #ignore inverted exp fit
      fitted > 0 ~ signal, #ignore cases which have linear fit > 0
      TRUE ~ signal - fitted
      ))
  } else {
    df %<>% dplyr::mutate(corrected = signal - fitted)
  }


  # plot fits to inspect
  p <- ggplot(df, aes(x = time, y = signal)) +
    geom_line(colour = "black") +
    geom_line(aes(y = fitted), colour = "red", linetype = "dashed") +
    geom_line(aes(y = corrected), colour = "blue") +
    annotate("text", label = animal_name, y = max(c(df$corrected,df$signal) + 0.1), x = 50) +
    theme_classic()

  if(show.plots) {
    print(p)
  }
  return(df %>% dplyr::rename(delF = corrected))
}

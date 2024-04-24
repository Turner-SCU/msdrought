#' @title Mid Summer Drought Function
#'
#' @description Generates all relevant statistics for the Mid Summer Drought by running the msdStats function for every
#' applicable metric. The output of msdMain is a dataframe containing every msdStats output for the available years of data.
#'
#' @usage msdMain(x, peakwindow1, minwindow1,
#' minwindow2, peakwindow2, quantity,
#' window, timeVector)
#'
#' @param x                 xts or vector of data
#' @param peakwindow1       desired date in MMDD format to begin analysis (window 1)
#' @param minwindow1        desired date in MMDD format to end analysis (window 1)
#' @param minwindow2        desired date in MMDD format to begin analysis (window 2)
#' @param peakwindow2       desired date in MMDD format to end analysis (window 2)
#' @param quantity          amount of times the filter is run
#' @param window            size of filter
#' @param timeVector        vector of dates (not needed for xts inputs)
#'
#' @returns Data frame of all relevant MSD Statistics
#'
#'
#' @examples
#'
#' data("timeseries")
#' ts <- timeseries
#' df <- msdrought::msdMain(ts)
#'
#' @export
#-------------------------------------------------------------------------------------------------------------------------------------------------
msdMain <- function(x, peakwindow1 = "05-01", minwindow1 = "06-01", minwindow2 = "08-31", peakwindow2 = "10-31", quantity = 2, window = 31, timeVector = 0) {
  # msdDates
  if (inherits(x, "timeseries") == TRUE) {
    timeVector <- stats::time(x)
    dates <- msdrought::msdDates(timeVector, peakwindow1, minwindow1, minwindow2, peakwindow2)
  } else if (inherits(x, "xts") == TRUE) {
    timeVector <- stats::time(x)
    dates <- msdrought::msdDates(timeVector, peakwindow1, minwindow1, minwindow2, peakwindow2)
  } else if (timeVector == 0) {
    stop("Error: no dates vector present")
  } else {
    dates <- msdrought::msdDates(timeVector, peakwindow1, minwindow1, minwindow2, peakwindow2)
  }
  #-------------------------------------------------------------------------------------------------------------------------------------------------
  # msdFilter
  x <- apply(x, MARGIN = 2, FUN = msdFilter, window = window, quantity = quantity)
  #-------------------------------------------------------------------------------------------------------------------------------------------------
  # msdStats
  durationValue <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn = "duration")
  intensityValue <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn = "intensity")
  firstMaxValue <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn = "firstMaxValue")
  secondMaxValue <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn = "secondMaxValue")
  minValue <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn = "min")
  #-------------------------------------------------------------------------------------------------------------------------------------------------
  # prepare output
  year1 <- lubridate::year(timeVector[1]) # find the first date of the provided date vector, x
  nyears <- floor(length(x) / 365) - 1
  years <- seq(from = year1, to = year1 + nyears, by = 1)
  yearsFrame <- data.frame(years)
  checkNA <- cbind(yearsFrame, durationValue, intensityValue, firstMaxValue, secondMaxValue, minValue)
  checkNA <- stats::na.omit(checkNA)
  colnames(checkNA) <- c("Years", "durationValue", "intensityValue", "firstMaxValue", "secondMaxValue", "minValue")

  origin <- timeVector[1]

  countDaysFrame <- data.frame(1:length(x))
  firstMaxFrame <- data.frame(match(x, checkNA$firstMaxValue))
  firstMaxFinal <- cbind(countDaysFrame, firstMaxFrame)
  firstMaxFinal <- stats::na.omit(firstMaxFinal)
  firstMaxDate <- as.character(origin + lubridate::days(firstMaxFinal$X1.length.x.))

  secondMaxFrame <- data.frame(match(x, checkNA$secondMaxValue))
  secondMaxFinal <- cbind(countDaysFrame, secondMaxFrame)
  secondMaxFinal <- stats::na.omit(secondMaxFinal)
  secondMaxDate <- as.character(origin + lubridate::days(secondMaxFinal$X1.length.x.))

  minFrame <- data.frame(match(x, checkNA$minValue))
  minFinal <- cbind(countDaysFrame, minFrame)
  minFinal <- stats::na.omit(minFinal)
  minDate <- as.character(origin + lubridate::days(minFinal$X1.length.x.))

  combined <- cbind(checkNA$Years, checkNA$durationValue, checkNA$intensityValue, checkNA$firstMaxValue, firstMaxDate, checkNA$secondMaxValue, secondMaxDate, checkNA$minValue, minDate)
  output <- data.frame(combined)
  colnames(output) <- c("years", "durationValue", "intensityValue", "firstMaxValue", "firstMaxDate", "secondMaxValue", "secondMaxDate", "minValue", "minDate")

  return(output)
}

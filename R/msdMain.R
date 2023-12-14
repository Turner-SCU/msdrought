#' @title Mid Summer Drought Function
#'
#' @description Generates all relevant statistics for the Mid Summer Drought
#'
#' @usage msdMain(x, peakWindow1, minWindow1,
#' minWindow2, peakWindow2, quantity,
#' window, time)
#'
#' @param x                 Vector or TimeSeries
#' @param peakWindow1       desired date in MMDD format to begin analysis (window 1)
#' @param minWindow1        desired date in MMDD format to end analysis (window 1)
#' @param minWindow2        desired date in MMDD format to begin analysis (window 2)
#' @param peakWindow2       desired date in MMDD format to end analysis (window 2)
#' @param time              Vector of dates (not needed for TimeSeries inputs)
#'
#' @return Data frame of all relevant MSD Statistics
#'
#'
#' @examples
#' # using timeseries (ts)
#' # output = msdMain(ts, peakWindow1="05-01", minWindow1="06-01",
#' # minWindow2 ="08-31", peakWindow2="10-31")
#'
#' @export
#-----------------------------------------------------------------------------------------------------------------------------------------
msdMain<-function(x, peakWindow1="05-01", minWindow1="06-01", minWindow2="08-31", peakWindow2="10-31", time=0){

# msdDates
  if (inherits(x, "timeseries")) {
  time = stats::time(x)
  dates = msdDates(time, peakWindow1, minWindow1, minWindow2, peakWindow2)
  } else if (inherits(x, "xts")) {
  time = stats::time(x)
  dates = msdDates(time, peakWindow1, minWindow1, minWindow2, peakWindow2)
  } else if (time == 0){
      print("Error: no dates vector present")
      stop
  } else {
    dates = msdDates(time, peakWindow1, minWindow1, minWindow2, peakWindow2)
  }

# msdStats
  duration <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn="duration")
  intensity <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn="intensity")
  firstMaxValue <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn="firstMaxValue")
  secondMaxValue <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn="secondMaxValue")
  min <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn="min")

  year1 = lubridate::year(time[1]) #find the first date of the provided date vector, x
  length = round(length(x)/365) - 1
  years = seq(from = year1, to = year1+length, by = 1)
  yearsFrame = data.frame(years)
  checkNA = cbind(yearsFrame, duration, intensity, firstMaxValue, secondMaxValue, min)
  checkNA = na.omit(checkNA)
  colnames(checkNA) = c("Years", "Duration", "Intensity", "firstMaxValue","secondMaxValue", "min")

  origin = time[1]

  countDaysFrame = data.frame(1:length(x))
  firstMaxFrame = data.frame(match(x, checkNA$firstMaxValue))
  firstMaxFinal = cbind(countDaysFrame, firstMaxFrame) %>%
    na.omit()
  firstMaxDate = as.character(origin + lubridate::days(firstMaxFinal$X1.length.x.))

  secondMaxFrame = data.frame(match(x, checkNA$secondMaxValue))
  secondMaxFinal = cbind(countDaysFrame, secondMaxFrame) %>%
    na.omit()
  secondMaxDate = as.character(origin + lubridate::days(secondMaxFinal$X1.length.x.))

  minFrame = data.frame(match(x,checkNA$min))
  minFinal = cbind(countDaysFrame, minFrame) %>%
    na.omit()
  minDate = as.character(origin + lubridate::days(minFinal$X1.length.x.))

  # prepare output
  combined = cbind(checkNA$Years, checkNA$Duration, checkNA$Intensity, checkNA$firstMaxValue, firstMaxDate, checkNA$secondMaxValue, secondMaxDate, checkNA$min, minDate)
  output = data.frame(combined)
  colnames(output) = c("Years", "Duration", "Intensity", "firstMaxValue", "firstMaxDate", "secondMaxValue", "secondMaxDate", "min", "minDate")

  return(output)
}

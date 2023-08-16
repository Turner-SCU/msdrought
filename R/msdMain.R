#' @title Mid Summer Drought Function
#'
#' @description Generates all relevant statistics for the Mid Summer Drought
#'
#' @usage msdMain(x, firstStartDate, firstEndDate,
#' secondStartDate, secondEndDate, quantity,
#' window, time)
#'
#' @param x                 Vector or TimeSeries
#' @param firstStartDate    desired date in MMDD format to begin analysis (window 1)
#' @param firstEndDate      desired date in MMDD format to end analysis (window 1)
#' @param secondStartDate   desired date in MMDD format to begin analysis (window 2)
#' @param secondEndDate     desired date in MMDD format to end analysis (window 2)
#' @param quantity          Amount of times the filter is run
#' @param window            Size of filter
#' @param time              Vector of dates (not needed for TimeSeries inputs)
#'
#' @return Data frame of all relevant MSD Statistics
#'
#'
#' @examples
#' # using timeseries (ts)
#' # output = msdMain(ts, firstStartDate="05-01", firstEndDate="08-31",
#' # secondStartDate ="06-01", secondEndDate="10-31", quantity = 2, window = 31)
#'
#' @export
#-----------------------------------------------------------------------------------------------------------------------------------------
msdMain<-function(x, firstStartDate="05-01", firstEndDate="06-01", secondStartDate="08-31", secondEndDate="10-31", quantity=2, window=31, time=0){

# msdDates
  if (inherits(x, "timeseries")) {
  time = stats::time(x)
  dates = msdDates(time, firstStartDate, firstEndDate, secondStartDate, secondEndDate)
  } else if (inherits(x, "xts")) {
  time = stats::time(x)
  dates = msdDates(time, firstStartDate, firstEndDate, secondStartDate, secondEndDate)
  } else if (time == 0){
      print("Error: no dates vector present")
      stop
  } else {
    dates = msdDates(time, firstStartDate, firstEndDate, secondStartDate, secondEndDate)
  }

# msdFilter
  for(i in 1:quantity){
    x = apply(x, MARGIN = 2, FUN = msdFilter, window = window)
  }

# msdStats
  duration <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn="duration")
  intensity <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn="intensity")
  firstMaxValue <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn="firstMaxValue")
  secondMaxValue <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn="secondMaxValue")
  min <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn="min")
  minDex <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn="mindex")

  origin = time[1]
  minDex = as.character(origin + minDex)

  firstMaxDate = 0
  secondMaxDate = 0

# prepare output
  year1 = lubridate::year(time[1]) #find the first date of the provided date vector, x
  length = round(length(xtimeseries)/365) - 1
  years = seq(from = year1, to = year1+length, by = 1)
  yearsFrame = data.frame(years)
  combined = cbind(yearsFrame, duration, intensity, firstMaxValue, firstMaxDate, secondMaxValue, secondMaxDate, min, minDex)
  output = data.frame(combined)
  colnames(output) = c("Years", "Duration", "Intensity", "firstMaxValue", "firstMaxDate", "secondMaxValue", "secondMaxDate", "min", "minDex")
  return(output)
}

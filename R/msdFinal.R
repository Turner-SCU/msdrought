#' @title Mid Summer Drought Function
#'
#' @description Generates the relevant statistics for the Mid Summer Drought
#'
#' @description HERE
#'
#' @usage msdFinal(x, firstStartDate, firstEndDate, secondStartDate, secondEndDate, quantity, window)
#'
#' @param x                 Date vector
#' @param firstStartDate    desired date in MMDD format to begin analysis (window 1)
#' @param firstEndDate      desired date in MMDD format to end analysis (window 1)
#' @param secondStartDate   desired date in MMDD format to begin analysis (window 2)
#' @param secondEndDate     desired date in MMDD format to end analysis (window 2)
#' @param quantity          Amount of times the filter is run
#' @param window            Size of filter
#'
#' @return Dataframe of all relevant MSD Statistics
#'
#'
#' @examples
#' # using timeseries (ts)
#' # output = msdFinal(ts, firstStartDate="05-01", firstEndDate="08-31",
#' # secondStartDate ="06-01", secondEndDate="10-31", quantity = 2, window = 31)
#'
#' @export
#-----------------------------------------------------------------------------------------------------------------------------------------
msdFinal<-function(x, firstStartDate="05-01", firstEndDate="06-01", secondStartDate="08-31", secondEndDate="10-31", quantity=2, window=31){

# msdDates
  if inherits(x, "timeseries") {
  time = stats::time(x)
  dates = msdDates(time, firstStartDate = "05-01", firstEndDate = "06-01", secondStartDate = "08-31", secondEndDate = "10-31")
  }

  else if (inherits(x, "xts") {
  time = stats::time(x)
  dates = msdDates(time, firstStartDate = "05-01", firstEndDate = "06-01", secondStartDate = "08-31", secondEndDate = "10-31")
  }

  else {
    request = readline("No dates found in the input file, please attach a dates sequence in the following form:
                       seq(from = as.Date('1981-01-01'), to = as.Date('1985-12-31'), by = 'day')")
    dates = msdDates(request, firstStartDate = "05-01", firstEndDate = "06-01", secondStartDate = "08-31", secondEndDate = "10-31")



# msdFilter
  for(i in 1:quantity){
    x = apply(x, MARGIN = 2, FUN = msdFilter, window = window)
  }

# msdStats
  duration <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn="duration")
  intensity <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn="intensity")
  firstMax <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn="firstMax")
  secondMax <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn="secondMax")
  min <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn="min")
  minDex <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn="mindex")

# prepare output
  combined = cbind(duration, intensity, firstMax, secondMax, min, minDex)
  colnames(combined) = c("Duration", "Intensity", "firstMax", "secondMax", "min", "minDex")
  output = data.frame(combined)
  return(output)
}

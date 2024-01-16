#' @title Mid Summer Drought Function
#'
#' @description Generates all relevant statistics for the Mid Summer Drought
#'
#' @usage msdMain(x, firstStartDate, firstEndDate,
#' secondStartDate, secondEndDate, quantity,
#' window, timeVector)
#'
#' @param x                 Vector or TimeSeries
#' @param firstStartDate    desired date in MMDD format to begin analysis (window 1)
#' @param firstEndDate      desired date in MMDD format to end analysis (window 1)
#' @param secondStartDate   desired date in MMDD format to begin analysis (window 2)
#' @param secondEndDate     desired date in MMDD format to end analysis (window 2)
#' @param quantity          Amount of times the filter is run
#' @param window            Size of filter
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
#-------------------------------------------------------------------------------------------------------------------------------------------------
msdMain<-function(x, firstStartDate="05-01", firstEndDate="06-01", secondStartDate="08-31", secondEndDate="10-31", quantity=2, window=31){

# msdDates
  if (inherits(x, "timeseries") == TRUE) {
    timeVector = stats::time(x)
    dates = msdDates(timeVector, firstStartDate, firstEndDate, secondStartDate, secondEndDate)
    ID = "ts"
  } else if (inherits(x, "xts") == TRUE) {
      timeVector = stats::time(x)
      dates = msdDates(timeVector, firstStartDate, firstEndDate, secondStartDate, secondEndDate)
      ID = "ts"
  } else if (inherits(x, "SpatRaster") == TRUE) {
      timeVector = terra::time(x)
      dates = msdDates(timeVector, firstStartDate, firstEndDate, secondStartDate, secondEndDate)
      ID = "sr"
  } else (givenTimeVector == 0){
      print("Error: format not recognized. Please see Vignettes for manual application")
      stop
  }
#-------------------------------------------------------------------------------------------------------------------------------------------------
# msdFilter
  if ((ID == "ts") == TRUE) {
    x = apply(x, MARGIN = 2, FUN = msdFilter, window = window, quantity = quantity)
  } else if ((ID == "sr") == TRUE) {
    x = terra::app(x, msdFilter, window = window, quantity = quantity)
  } else{
    print("Error: format not recognized. Please see Vignettes for manual application")
  }
#-------------------------------------------------------------------------------------------------------------------------------------------------
# msdStats
  if ((ID == "ts") == TRUE) {
    durationValue <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn="duration")
    intensityValue <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn="intensity")
    firstMaxValue <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn="firstMaxValue")
    secondMaxValue <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn="secondMaxValue")
    minValue <- apply(x, MARGIN = 2, FUN = msdStats, dates, fcn="min")
  } else if ((ID == "sr") == TRUE) {
    durationValue = terra::app(x, msdStats, dates = dates, fcn="duration")
    intensityValue = terra::app(x, msdStats, dates = dates, fcn="intensity")
    firstMaxValue = terra::app(x, msdStats, dates = dates, fcn="firstMaxValue")
    secondMaxValue = terra::app(x, msdStats, dates = dates, fcn="secondMaxValue")
    minValue = terra::app(x, msdStats, dates = dates, fcn="min")

  } else{
    print("Error: format not recognized. Please see Vignettes for manual application")
  }

#-------------------------------------------------------------------------------------------------------------------------------------------------
# prepare output
  firstYear = lubridate::year(timeVector[1]) #find the first date of the provided date vector, x
  finalYear = lubridate::year(timeVector[length(timeVector)]) #find the final date of the provided date vector
  years = seq(from = firstYear, to = finalYear, by = 1)
  yearsFrame = data.frame(years)
  checkNA = cbind(yearsFrame, durationValue, intensityValue, firstMaxValue, secondMaxValue, minValue) #!!! Something needs to give here
  checkNA = na.omit(checkNA)
  colnames(checkNA) = c("Years", "durationValue", "intensityValue", "firstMaxValue","secondMaxValue", "minValue")

  origin = timeVector[1]

  countDaysFrame = data.frame(1:length(x))
  firstMaxFrame = data.frame(match(x, checkNA$firstMaxValue))
  firstMaxFinal = cbind(countDaysFrame, firstMaxFrame) %>%
    na.omit()
  firstMaxDate = as.character(origin + lubridate::days(firstMaxFinal$X1.length.x.))

  secondMaxFrame = data.frame(match(x, checkNA$secondMaxValue))
  secondMaxFinal = cbind(countDaysFrame, secondMaxFrame) %>%
    na.omit()
  secondMaxDate = as.character(origin + lubridate::days(secondMaxFinal$X1.length.x.))

  minFrame = data.frame(match(x,checkNA$minValue))
  minFinal = cbind(countDaysFrame, minFrame) %>%
    na.omit()
  minDate = as.character(origin + lubridate::days(minFinal$X1.length.x.))

  combined = cbind(checkNA$Years, checkNA$durationValue, checkNA$intensityValue, checkNA$firstMaxValue, firstMaxDate, checkNA$secondMaxValue, secondMaxDate, checkNA$minValue, minDate)
  output = data.frame(combined)
  colnames(output) = c("years", "durationValue", "intensityValue", "firstMaxValue", "firstMaxDate", "secondMaxValue", "secondMaxDate", "minValue", "minDate")

  return(output)
}

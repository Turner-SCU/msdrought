#' @title Main Mid Summer Drought Calculation Function
#'
#' @description This function calculates the different statistics of the mid summer drought from a Time Series.
#' The input must be in the form of daily data, with the first data point being January 1st of a respective year.
#'
#' @usage msdStats(x, dates, fcn)
#'
#' @param x         Filtered xts data (from msdFilter)
#' @param dates     Vector of Dates (from the msdDates function)
#' @param fcn       Specify what values to be pulled from the function.
#' Options are 'duration', 'intensity', 'firstMaxValue', 'secondMaxValue', 'min', 'mindex'.
#'
#' @returns SpatRaster or TimeSeries of Yearly data
#'
#' @examples
#'
#' data("timeseries")
#' ts <- timeseries
#' dates <- zoo::index(ts)
#' filteredData <- msdrought::msdFilter(ts, window = 31, quantity = 2)
#' keyDates <- msdDates(dates)
#' msdrought::msdStats(filteredData, keyDates, fcn = "duration")
#'
#' @export
#'
#-----------------------------------------------------------------------------------------------------------------------------------------
msdStats <- function(x, dates, fcn) {
  # check for valid arguments
  if (missing(dates)) {
    stop("missing dates argument in msdStats function")
  }
  if (!(fcn %in% c("duration", "intensity", "firstMaxValue", "secondMaxValue", "min", "mindex"))) {
    stop("fcn must be one of duration, intensity, firstMaxValue, secondMaxValue, min, mindex")
  }
  #-----------------------------------------------------------------------------------------------------------------------------------------
  data <- as.numeric(x) # making sure the data is numeric
  peaks <- quantmod::findPeaks(data) - 1 # finding all of the peaks of the data
  valleys <- quantmod::findValleys(data) - 1 # finding all of the valleys of the data
  output <- c(0) # creating a new variable
  #-----------------------------------------------------------------------------------------------------------------------------------------
  # Pull the values for the critical MSD dates (formerly ipdates2, msdDates)
  criticalDates <- c(0)
  for (i in 1:length(dates)) {
    if (dates[i] == 1) {
      break
    } else {
      criticalDates <- c(criticalDates, dates[i])
    }
  }
  criticalDates <- criticalDates[-c(1)]
  # Pull the values for the start and end of each year (formerly ipdates4, msdYear)
  yearDates <- c(0)
  for (j in i:length(dates)) {
    yearDates <- c(yearDates, dates[j])
  }
  yearDates <- yearDates[-c(1)]
  #-----------------------------------------------------------------------------------------------------------------------------------------
  nyears <- floor(length(x) / 365)
  for (years in 1:nyears) { # running for every year
    date1 <- criticalDates[4 * years - 2] # the next six lines just pull the proper indices
    date2 <- criticalDates[4 * years - 1]
    date3 <- criticalDates[4 * years - 3]
    date4 <- criticalDates[4 * years]
    date5 <- yearDates[2 * years - 1]
    date6 <- yearDates[2 * years]
    # checking for min valley between the inner dates
    min1 <- min(data[valleys[date1 <= valleys & valleys <= date2]], na.rm = TRUE)
    # checking for min valley between the outer dates
    min2 <- min(data[valleys[date3 <= valleys & valleys <= date4]], na.rm = TRUE)
    mindex <- which.min(data[valleys[date1 <= valleys & valleys <= date2]])
    mindate <- valleys[date1 <= valleys & valleys <= date2][mindex]
    mindex2 <- which.min(data[valleys[date3 <= valleys & valleys <= date4]])
    mindate2 <- valleys[date3 <= valleys & valleys <= date4][mindex2]
    check1 <- mindate == mindate2 # making sure that the index does overlap
    if (anyNA(mindate) == TRUE | length(mindate) == 0) { # making sure we have a minimum, otherwise an NA is output #!!ERROR: argument is of length zero
      output[years] <- NA
    } else {
      datespk <- c(peaks[date3 <= peaks & peaks <= date4], mindate) # finding all the peaks between the outer dates
      datespk <- sort(datespk) # sorting them in order with the mindate
      mindex3 <- which.min(data[datespk]) # finding the index of the mindate
      maxdex1 <- datespk[1:(mindex3 - 1)] # the next few lines find the max before the minimum and after
      maxdex2 <- datespk[(mindex3 + 1):length(dates)]
      maxpos1 <- data[maxdex1]
      maxpos2 <- data[maxdex2]
      max1 <- max(maxpos1, na.rm = TRUE)
      max2 <- max(maxpos2, na.rm = TRUE)
      pos1 <- which.max(maxpos1)
      pos2 <- which.max(maxpos2)
      index1 <- maxdex1[pos1]
      index2 <- maxdex2[pos2]
      maxcheck1 <- max(data[date5:mindate], na.rm = TRUE) # making sure that the max is the real between january and mindate
      maxcheck2 <- max(data[mindate:date6], na.rm = TRUE) # making sure that the max is the real between mindate and december
      maxval1 <- max1 == maxcheck1
      maxval2 <- max2 == maxcheck2
      max1 <- max1 * maxval1
      max2 <- max2 * maxval2
      if (anyNA(check1) == TRUE) { # making sure that the minimum is the minimum
        output[years] <- NA
      } else if (length(max1) == 0) { # the next couple ensure that we have values to pull from
        output[years] <- NA
      } else if (length(max2) == 0) {
        output[years] <- NA
      } else if (anyNA(max1) == TRUE) {
        output[years] <- NA
      } else if (anyNA(max2) == TRUE) {
        output[years] <- NA # !! This is the case for 1981, what should happen here?
      } else if (max1 == 0) {
        output[years] <- NA
      } else if (max2 == 0) {
        output[years] <- NA
      } else if (fcn == "duration") { # the different cases to choose from for 'fcn'
        output[years] <- index2 - index1
      } else if (fcn == "intensity") {
        output[years] <- ((max1 + max2) / 2) - min1
      } else if (fcn == "firstMaxValue") {
        output[years] <- max1
      } else if (fcn == "secondMaxValue") {
        output[years] <- max2
      } else if (fcn == "min") {
        output[years] <- min1
      } else if (fcn == "mindex") {
        output[years] <- index1
      } else {
        output[years] <- NA
      }
    }
  } # end of For years loop
  return(c(output))
}

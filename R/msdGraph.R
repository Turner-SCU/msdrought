#' @title Mid Summer Drought Time Series Graphs
#'
#' @description Plots the Time Series of Mid Summer Drought data. The input must be in the form of daily data,
#' with the first data point being January 1st of a respective year.
#'
#' @usage msdGraph(x, year, peakwindow1, minwindow1,
#' minwindow2, peakwindow2, quantity,
#' window, timeVector)
#'
#' @param x               vector of data or xts
#' @param year            year of interest
#' @param peakwindow1     date in MMDD format to begin analysis (window 1)
#' @param minwindow1      date in MMDD format to end analysis (window 1)
#' @param minwindow2      date in MMDD format to begin analysis (window 2)
#' @param peakwindow2     date in MMDD format to end analysis (window 2)
#' @param quantity        number of times the filter is to be run
#' @param window          size of filter
#' @param timeVector      vector of dates (not needed for xts inputs)
#'
#' @returns Graph of Time Series Data
#'
#' @examples
#'
#' \dontrun{
#' data("timeseries")
#' ts <- timeseries
#' msdrought::msdGraph(ts, 1982)
#' }
#'
#' @export
#-----------------------------------------------------------------------------------------------------------------------------------------
msdGraph <- function(x, year, peakwindow1 = "05-01", minwindow1 = "06-01",
                     minwindow2 = "08-31", peakwindow2 = "10-31", quantity = 2,
                     window = 31, timeVector = 0) {
  #-----------------------------------------------------------------------------------------------------------------------------------------
  # Define variables as NULL values to begin with
  years <- NULL
  Date <- NULL
  Precipitation <- NULL
  #-----------------------------------------------------------------------------------------------------------------------------------------
  # Assemble the base timeseries to be plotted (if the input "x" is not already a timeseries)
  if (inherits(x, "timeseries")) {
    timeseriesFull <- x
  } else if (inherits(x, "xts")) {
    timeseriesFull <- x
  } else {
    datesSeq <- NULL # !!!
    timeseriesFull <- xts::xts(x, datesSeq)
  }
  # Calculate all stats for all years of the provided data, then select only the relevant year's data.
  allStats <- msdrought::msdMain(x, peakwindow1, minwindow2, minwindow1, peakwindow2, quantity, window, timeVector)
  yearStats <- subset(allStats, years == year)
  # Subset the timeseries to only be the year (365 days) of interest
  allDates <- seq(from = as.Date(stats::time(timeseriesFull[1])), to = as.Date(stats::time(timeseriesFull[length(timeseriesFull)])), by = 1)
  yearsOnly <- format(allDates, "%Y")
  validDates <- (year == yearsOnly)
  tsDates <- allDates[validDates == TRUE]
  # Filter the data for the purpose of graphing
  timeseriesFull <- apply(timeseriesFull, MARGIN = 2, FUN = msdFilter, window = window, quantity = quantity)
  timeseriesPrecip <- timeseriesFull[validDates == TRUE]
  timeseriesFrame <- data.frame(as.Date(tsDates), as.numeric(timeseriesPrecip))
  colnames(timeseriesFrame) <- c("Date", "Precipitation")
  # Create variables for the firstStart, secondStart, firstEnd, and secondEnd MM-DD inputs in YY-MM-DD format (add year of interest)
  pw1 <- as.Date(paste(year, peakwindow1, sep = "-"))
  mw2 <- as.Date(paste(year, minwindow2, sep = "-"))
  mw1 <- as.Date(paste(year, minwindow1, sep = "-"))
  pw2 <- as.Date(paste(year, peakwindow2, sep = "-"))
  #-----------------------------------------------------------------------------------------------------------------------------------------
  # Assign individual variables to each important column value from the yearStats dataframe (from msdMain)
  durationVal <- as.numeric(yearStats[2])
  intensityVal <- as.numeric(yearStats[3])
  firstMaxVal <- as.numeric(yearStats[4])
  secondMaxVal <- as.numeric(yearStats[6])
  minVal <- as.numeric(yearStats[8])
  # Create data frames of the dates and values corresponding to the maximums and minimums
  firstMax <- data.frame(yearStats[5], firstMaxVal)
  colnames(firstMax) <- c("Date", "Precipitation")
  secondMax <- data.frame(yearStats[7], secondMaxVal)
  colnames(secondMax) <- c("Date", "Precipitation")
  theMin <- data.frame(yearStats[9], minVal)
  colnames(theMin) <- c("Date", "Precipitation")

  durat <- function(x, y = 15) {
    x[x < y] <- 0
    x[x > y - 1] <- 1
    return(x)
  }
  durationX <- durat(durationVal)

  inten <- function(x, y = 3) {
    x[x < y] <- 0
    x[x >= y] <- 1
    return(x)
  }
  intensityX <- inten(intensityVal)

  DIsum <- as.numeric(durationX) + as.numeric(intensityX)

  colors <- function(c) {
    c[c != 2] <- "white"
    c[c == 2] <- "lightblue"
    return(c)
  }
  color1 <- colors(DIsum)
  #-----------------------------------------------------------------------------------------------------------------------------------------
  output <- ggplot2::ggplot(data = timeseriesFrame, mapping = ggplot2::aes(x = Date, y = Precipitation)) +
    ggplot2::theme_bw() +
    ggplot2::annotate("rect", xmin = timeseriesFrame[1, 1], xmax = timeseriesFrame[length(tsDates), 1], ymin = 0, ymax = (max(firstMaxVal, secondMaxVal) + 2), fill = color1[1], alpha = 0.5) +
    ggplot2::geom_line(data = timeseriesFrame[1:nrow(timeseriesFrame), ]) +
    ggplot2::geom_point(data = firstMax, mapping = ggplot2::aes(x = as.Date(Date), y = Precipitation), size = 2, color = "red") +
    ggplot2::geom_point(data = secondMax, mapping = ggplot2::aes(x = as.Date(Date), y = Precipitation), size = 2, color = "red") +
    ggplot2::geom_point(data = theMin, mapping = ggplot2::aes(x = as.Date(Date), y = Precipitation), size = 2, color = "blue") +
    ggplot2::geom_vline(xintercept = pw1, color = "red") +
    ggplot2::geom_vline(xintercept = mw2, color = "blue") +
    ggplot2::geom_vline(xintercept = mw1, color = "blue") +
    ggplot2::geom_vline(xintercept = pw2, color = "red") +
    ggplot2::xlab("") +
    ggplot2::ylab("Precipitation (mm/day)") +
    ggplot2::scale_y_continuous(breaks = seq(0, (max(firstMaxVal, secondMaxVal) + 2), by = 5), limits = c(0, (max(firstMaxVal, secondMaxVal) + 2)), expand = c(0, 0)) +
    ggplot2::theme(text = ggplot2::element_text(size = 20))
  return(output)
}

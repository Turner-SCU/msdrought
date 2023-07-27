#' @title Bartlett Noise Filter
#'
#' @description A triangular Filter for smoothing data
#' The filter will take a weighted average of a specified number of points
#' around the point of interest to create a smoother time series. For
#' example, if the size of the filter is set to 31, the filter will take the
#' 15 points before and after the point of interest (for a total of 31 points),
#' and calculate a weighted average based on how far away the points are from
#' the point of interest.
#'
#' @usage msdFilter(x, window, quantity)
#'
#' @param x          RasterBrick, TimeSeries
#' @param window     Size of Filter, default window is 31
#' @param quantity   The amount of times the filter will be run, default quantity is 2
#'
#' @return RasterBrick or TimeSeries of Yearly data
#'
#' @seealso \code{\link{msd}}
#'
#' @examples
#' # using spatRaster or a Time Series
#' r<-msdFilter(x, window = 31, quantity = 2)
#'
#' @export
#'
#-----------------------------------------------------------------------------------------------------------------------------------------
msdFilter <- function(x, window=31, quantity=2) {
  #apply filter weighted on the average and divided by the sum of the bartlett window to smooth out the data
  bartlett_noise_filter = function(x, window) {
    #constructs a Bartlett vector with the size of the bartlett filter
    bartlett_window <- c(signal::bartlett(window))
    #creates a sum of the bartlett window to construct an average
    bartlett_sum <- sum(bartlett_window)
    #calculate the necessary fraction of the window and sum
    ratio = bartlett_window/bartlett_sum
    filtered = stats::filter(x, ratio, method="convolution")
  }
  #Find the format of the provided file "x". It should either be a raster or a timeseries
  format = c(0) #Set up an empty variable for future modification
  if (as.character(tryCatch(rast(x), error = function(e) FALSE)) == "FALSE") {
    rasterCheck = FALSE
  } else {
    rasterCheck = TRUE
  }
  if (tryCatch(as.xts(x), error = function(e) FALSE) == "FALSE") {
    timeseriesCheck = FALSE
  } else {
    timeseriesCheck = TRUE
  }

  if ((rasterCheck == timeseriesCheck)) {
    print("error! input file is not recognized as a raster or timeseries")
    format = "error"
    break
  }
  if (rasterCheck == FALSE && timeseriesCheck == TRUE) {
    format = "timeseries"
  } else if (timeseriesCheck == FALSE && rasterCheck == TRUE) {
    format = "raster"
  }
  #filter the data for the amount of times specified
  if (format == "raster") {
    for (i in 1:quantity){ #run the loop as many times as desired
      x <- terra::app(x, bartlett_noise_filter, window=window)
  }
  }
    else if (format == "timeseries") {
      x = data.frame(timeseries)
      x[] = apply(x, MARGIN = 2, FUN = bartlett_noise_filter, window = window)
      xtime = time(timeseries)
      x = data.frame(x, xtime)
      colnames(x) = c("Precipitation", "Date")
      x = xts(x$Precipitation, x$Date)
    }
    else if (format == "error") {
      print("error! the input file is not a raster or timeseries")
    }
  return(x)
}

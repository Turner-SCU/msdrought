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
#' # using Spatrast or a Time Series
#' #r<-msdFilter(x, window = 31, quantity = 2)
#'
#' @export
#'
#-----------------------------------------------------------------------------------------------------------------------------------------
msdFilter <- function(x, window=31, quantity=2) {

  #constructs a Bartlett vector with the size of the bartlett filter
  bartlett_window <- c(signal::bartlett(window))
  #creates a sum of the bartlett window to construct an average
  bartlett_sum <- sum(bartlett_window)
  #calculate the necessary fraction of the window and sum
  ratio = bartlett_window/bartlett_sum

  #apply filter weighted on the average and divided by the sum of the bartlett window to smooth out the data
  bartlett_noise_filter = function(x, window) {
    filtered_data = stats::filter(x, ratio, method="convolution")
  }

  for (i in 1:quantity){ #run the loop as many times as desired
    filtered <- terra::app(filtered_data, bartlett_noise_filter, window=window)
  }
  return(filtered)
}

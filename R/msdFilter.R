#' Bartlett Noise Filter -- A triangular Filter for smoothing data
#' The filter will take a weighted average of a specified number of points
#' around the point of interest to create a smoother time series. For
#' example, if the size of the filter is set to 31, the filter will take the
#' 15 points before and after the point of interest (for a total of 31 points),
#' and calculate a weighted average based on how far away the points are from
#' the point of interest.
#'
#' @param x          RasterBrick or TimeSeries
#' @param window     Size of Filter
#'
#' @return RasterBrick or TimeSeries of Yearly data
#'
#' @seealso \code{\link{msd}}
#'
#' @examples
#' # using Spatrast or a Time Series
#' #r<-bartlett_noise_filter(x, window = 31)
#'
#' @export
#'
bartlett_noise_filter <- function(x, window) {
  #constructs a Bartlett vector with the size of the bartlett filter
  bartlett_window <- c(signal::bartlett(window))
  #creates a sum of the bartlett window to construct an average
  bartlett_sum <- sum(bartlett_window)
  ratio = bartlett_window/bartlett_sum

  #apply filter weighted on the average and divided by the sum of the bartlett window to smooth out the data
  filtered_data <- stats::filter(x, ratio, method="convolution")
  return(filtered_data)
}


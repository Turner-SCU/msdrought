#' msdFilter -- A triangular Filter for smoothing data
#'
#' @description The filter will take a weighted average of a specified number of points
#' around the point of interest to create a smoother time series. For
#' example, if the size of the filter is set to 31, the filter will take the
#' 15 points before and after the point of interest (for a total of 31 points),
#' and calculate a weighted average based on how far away the points are from
#' the point of interest.
#'
#' @usage msdFilter(x, window = 31, quantity = 2)
#'
#' @param x           RasterBrick or TimeSeries
#' @param window      Size of Filter [Default = 31]
#' @param quantity    Number of passes to apply filter [Default = 2]
#'
#' @return RasterBrick or TimeSeries of Yearly data
#'
#'
#' @examples
#' # using Spatrast or a Time Series
#' #r<-msdFilter(x, window = 31)
#'
#' @export
#'
#-----------------------------------------------------------------------------------------------------------------------------------------
msdFilter <- function(x, window = 31, quantity = 2) {
  #constructs a Bartlett vector with the size of the bartlett filter
  bartlett_window <- c(signal::bartlett(window))
  #creates a sum of the bartlett window to construct an average
  bartlett_sum <- sum(bartlett_window)

  #apply filter weighted on the average and divided by the sum of the bartlett window to smooth out the data, and return an xts object
  timeFrame = terra::time(x) %>%
    data.frame()
  filtered_data <- as.vector(x) # coerce to vector if it is a timeseries
  for (i in 1:quantity) {
    filtered_data <- stats::filter(filtered_data,bartlett_window/bartlett_sum,method="convolution")
  }
  timeseriesFrame = cbind(timeFrame, filtered_data)
  colnames(timeseriesFrame) = c("Date", "Precipitation")
  allFiltered = xts(timeseriesFrame$Precipitation, timeseriesFrame$Date) #This produces the "xts" screenshot
  return(allFiltered)
}


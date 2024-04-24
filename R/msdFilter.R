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
#' @param x           TimeSeries or numeric vector
#' @param window      Size of Filter [Default = 31]
#' @param quantity    Number of passes to apply filter [Default = 2]
#'
#' @returns Vector of Yearly data
#'
#' @examples
#'
#' dates <- seq(from = as.Date("1981-01-01"), to = as.Date("1982-12-31"), by = "day")
#' ts <- xts::xts(runif(length(dates), 0, 50),dates)
#' filteredData <- msdrought::msdFilter(ts, window = 31, quantity = 2)
#'
#' @export
#'
#-----------------------------------------------------------------------------------------------------------------------------------------
msdFilter <- function(x, window = 31, quantity = 2) {
  # constructs a Bartlett vector with the size of the bartlett filter
  bartlett_window <- c(signal::bartlett(window))
  # creates a sum of the bartlett window to construct an average
  bartlett_sum <- sum(bartlett_window)
  weights <- bartlett_window / bartlett_sum

  # apply filter weighted on the average and divided by the sum of the bartlett window to smooth out the data
  filtered_data <- as.vector(x) # coerce to vector if it is a timeseries
  for (i in 1:quantity) {
    filtered_data <- stats::filter(filtered_data, weights, method = "convolution", circular = TRUE)
  }
  return(as.vector(filtered_data))
}

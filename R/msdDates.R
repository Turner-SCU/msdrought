#' @title Finding indices for Dates Relevant to the MSD Calculations
#'
#' @description This function pulls the indices associated with dates that correspond
#' with the Mid Summer Drought, and is used in conjunction with the MSD function.
#' It also pulls the first and last day of each calendar year.
#'
#' @usage msdDates(x, peakwindow1, minwindow1, minwindow2, peakwindow2)
#'
#' @param x                 Date vector
#' @param peakwindow1       desired date in MMDD format to begin search for early peak
#' @param minwindow1        desired date in MMDD format to begin search for minimum
#' @param minwindow2        desired date in MMDD format to end search for minimum
#' @param peakwindow2       desired date in MMDD format to end search for late peak
#'
#' @return Date vector containing each year's beginning date, end date, and critical MSD dates
#'
#' @examples
#' x <- seq(from = as.Date("1981-01-01"), to = as.Date("1985-12-31"), by = "day")
#' msdDates(x, peakwindow1="05-01",minwindow1="06-01",minwindow2 ="08-31",peakwindow2="10-31")
#'
#' @export
#'
#-----------------------------------------------------------------------------------------------------------------------------------------
msdDates <- function(x, peakwindow1 = "05-01", minwindow1 = "06-01", minwindow2 = "08-31", peakwindow2 = "10-31"){

  #Check that data begin on Jan 1
  if((format(x[1], "%m-%d") != "01-01")) {
    stop("current function requires a January 1 start date\n")
  }

  #find indices for all years for key dates
  jan01 <- which(format(x,"%m-%d") == "01-01")
  dec31 <- which(format(x,"%m-%d") == "12-31")
  pw1 <- which(format(x,"%m-%d") == peakwindow1)
  mw1 <- which(format(x,"%m-%d") == minwindow1)
  mw2 <- which(format(x,"%m-%d") == minwindow2)
  pw2 <- which(format(x,"%m-%d") == peakwindow2)

  #make sure lengths are the same
  if (!(all.equal(length(pw1),length(pw2),length(mw1),length(mw2),length(jan01),length(dec31)))) {
    stop("Date vectors have different lengths, possible incomplete years\n")
  }

  #sort the indices to assure compatibility for the msdStats function
  kDates = c(pw1, mw1, pw2, mw2) %>%
    sort(decreasing = FALSE)
  kYears = c(jan01, dec31) %>%
    sort(decreasing = FALSE)
  k <- as.vector(c(kDates, kYears))

  return(k)
}

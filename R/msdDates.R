#' @title Pulling Dates Relevant to the MSD Calculations
#'
#' @description This function pulls the dates that correspond with the Mid Summer Drought, and is used in conjuction with the MSD function.
#' It also pulls the first and last day of each calendar year provided.

#'
#' @usage msdDates(x, firstStartDate, firstEndDate, secondStartDate, secondEndDate)
#'
#' @param x                 vector of dates
#' @param firstStartDate    desired date in MMDD format to begin analysis (window 1)
#' @param firstEndDate      desired date in MMDD format to end analysis (window 1)
#' @param secondStartDate   desired date in MMDD format to begin analysis (window 2)
#' @param secondEndDate     desired date in MMDD format to end analysis (window 2)
#'
#' @return Date vector containing each year's beginning date, end date, and critical MSD dates
#'
#' @examples
#' x <- seq(from = as.Date("1981-01-01"), to = as.Date("1985-12-31"), by = "day")
#' msdDates(x, firstStartDate="05-01",firstEndDate="08-31",
#' secondStartDate ="06-01",secondEndDate="10-31")
#'
#' @export
#'
#-----------------------------------------------------------------------------------------------------------------------------------------
msdDates <- function(x, firstStartDate = "05-01", firstEndDate = "08-31", secondStartDate = "06-01", secondEndDate = "10-31"){

  #Find the pre-set date bounds for the MSD (define the first year of the data, January 1, and December 31)
  year1 = lubridate::year(x[1]) #find the first date of the provided date vector, x
  date1="01-01" #January 1st, the first day of the year
  date2="12-31" #December 31st, the first day of the year

  #Calculate the MSD boundaries as well as the first and last days of the year
  i<-year1
  l<-round(length(x)/365)
  j<-i+l-1
  kDates<-c(0) #Values associated with key MSD dates
  kYear<-c(0) #Values for the first and last days of a year

  for(year in i:j)
  {
    kDates[((year-i)*4)+1]<-which(grepl(paste(as.character(year), firstStartDate, sep="-"),x))
    kDates[((year-i)*4)+2]<-which(grepl(paste(as.character(year), firstEndDate, sep="-"),x))
    kDates[((year-i)*4)+3]<-which(grepl(paste(as.character(year), secondStartDate, sep="-"),x))
    kDates[((year-i)*4)+4]<-which(grepl(paste(as.character(year), secondEndDate, sep="-"),x))
    kYear[((year-i)*2)+1]<-which(grepl(paste(as.character(year),date1,sep="-"),x))
    kYear[((year-i)*2)+2]<-which(grepl(paste(as.character(year),date2,sep="-"),x))
  }
  k = c(kDates, kYear)
  return(k)
}

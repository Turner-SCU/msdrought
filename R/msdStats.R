#' @title Main Mid Summer Drought Calculation Function
#'
#' @description This function calculates the different statistics of the mid summer drought from a RasterBrick or a Time Series.
#'
#' The input must be in the form of daily data, with the first data point being January 1st of a respective year.
#' If x is a SpatRaster, then the output is a SpatRaster with a data point for each year.
#'
#' @usage msdStats(x, dates, fcn)
#'
#' @param x         xts object
#' @param dates     Vector of Dates (from the msdDates function)
#' @param fcn       Specify what values to be pulled from the function.
#' Options are 'duration', 'intensity', 'firstMaxValue', 'secondMaxValue', 'min', 'mindex'.
#' @param quantity  Amount of times the filter is run (prior to determining the stats)
#' @param window    Size of filter (prior to determining the stats)
#'
#' @return SpatRaster or TimeSeries of Yearly data
#'
#' @examples
#' # using spatRaster
#' # r<-terra::app(raster, msdStats, dates = d1, fcn="duration")
#'
#' @export
#'
#-----------------------------------------------------------------------------------------------------------------------------------------
msdStats <- function(x, dates, fcn, quantity=2, window=31){
  #check for valid arguments
  if(missing(dates)) {
    stop("missing dates argument in msdStats function")
  }
  if(!( fcn %in% c('duration', 'intensity', 'firstMaxValue', 'secondMaxValue', 'min', 'mindex'))){
    stop("fcn must be one of duration, intensity, firstMaxValue, secondMaxValue, min, mindex")
  }
  #-----------------------------------------------------------------------------------------------------------------------------------------
  # msdFilter and dates generation
  times = time(x)
  filtered = x
  for(i in 1:quantity){
    filtered = apply(filtered, MARGIN = 2, FUN = msdFilter, window = window)
  }
  data<-c(as.numeric(filtered)) #making sure the data is numeric
  peaks<-quantmod::findPeaks(data)-1 #finding all of the peaks of the data
  valleys<-quantmod::findValleys(data)-1 #finding all of the valleys of the data
  output<-c(0) #creating a new variable
  #-----------------------------------------------------------------------------------------------------------------------------------------
  nyears <- ceiling(lubridate::interval(times[1], times[length(times)]) / lubridate::years(1))
  for (years in 1:nyears){ #running for every year #running for every year
    date1 = dates[6*years-3] #the next six lines just pull the proper indices
    date2 = dates[6*years-2]
    date3 = dates[6*years-4]
    date4 = dates[6*years-1]
    date5 = dates[6*years-5]
    date6 = dates[6*years]
    #checking for min valley between the inner dates
    min<-min(data[valleys[date1<=valleys & valleys<=date2]],na.rm=TRUE)
    #checking for min valley between the outer dates
    min2<-min(data[valleys[date3<= valleys & valleys<=date4]],na.rm=TRUE)

    mindate = which.min(data[valleys[date1<=valleys & valleys<=date2]],na.rm=TRUE) #finding the index of min
    mindate2 = which.min(data[valleys[date3<= valleys & valleys<=date4]],na.rm=TRUE)
    check1<-mindate==mindate2 #making sure that the index does overlap
    if (is.na(mindate)==TRUE){ #making sure we have a minimum, otherwise an NA is output
      output[years]<-NA
    }else{
      dates<-c(peaks[date3<=peaks & peaks<=date4], mindate) #finding all the peaks between the outer dates
      dates<-sort(dates) #sorting them in order with the mindate
      mindex<-match(mindate,dates) #finding the index of the mindate
      maxdex1<-dates[1:(mindex-1)] #the next few lines find the max before the minimum and after
      maxdex2<-dates[(mindex+1):length(dates)]
      maxpos1<-data[maxdex1]
      maxpos2<-data[maxdex2]
      max1<-max(maxpos1,na.rm=TRUE)
      max2<-max(maxpos2,na.rm=TRUE)
      pos1<-match(max1,maxpos1)
      pos2<-match(max2,maxpos2)
      index1<-maxdex1[pos1]
      index2<-maxdex2[pos2]
      maxcheck1<-max(data[date5:mindate],na.rm=TRUE) #making sure that the max is the real between january and mindex
      maxcheck2<-max(data[mindate:date6],na.rm=TRUE) #making sure that the max is the real between mindex and december
      maxval1<-max1==maxcheck1
      maxval2<-max2==maxcheck2
      max1<-max1*maxval1
      max2<-max2*maxval2
      if (is.na(check1)==TRUE){ #making sure that the minimum is the minimum
        output[years]<-NA
      }else if (length(max1)==0){#the next couple ensure that we have values to pull from
        output[years]<-NA
      }else if (length(max2)==0){
        output[years]<-NA
      }else if (is.na(max1)==TRUE){
        output[years]<-NA
      }else if (is.na(max2)==TRUE){
        output[years]<-NA
      }else if (max1==0){
        output[years]<-NA
      }else if (max2==0){
        output[years]<-NA
      }
      else if (fcn=="duration"){ #the different cases to choose from for 'fcn'
        output[years]<-index2-index1
      }else if (fcn=="intensity"){
        output[years]<-((max1+max2)/2)-min
      }else if (fcn=="firstMaxValue"){
        output[years]<-max1
      }else if (fcn=="secondMaxValue"){
        output[years]<-max2
      }else if (fcn=="min"){
        output[years]<-min
      }else if (fcn=="mindex"){
        output[years]<-index1
      }else
        output[years]<-NA
    }
  } #end of For years loop
  return(as.vector(output))
}

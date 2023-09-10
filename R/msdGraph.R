#' @title Mid Summer Drought Time Series Graphs
#'
#' @description Plots the Time Series of Mid Summer Drought data
#'
#' @description The input must be in the form of daily data, with the first data point being January 1st of a respective year.
#'
#' @usage msdGraph(x, year, firstStartDate, firstEndDate,
#' secondStartDate, secondEndDate, quantity,
#' window, time)
#'
#' @param x                 Vector or TimeSeries
#' @param year              Year of interest
#' @param firstStartDate    desired date in MMDD format to begin analysis (window 1)
#' @param firstEndDate      desired date in MMDD format to end analysis (window 1)
#' @param secondStartDate   desired date in MMDD format to begin analysis (window 2)
#' @param secondEndDate     desired date in MMDD format to end analysis (window 2)
#' @param quantity          Amount of times the filter is run
#' @param window            Size of filter
#' @param time              Vector of dates (not needed for TimeSeries inputs)
#'
#' @return Graph of Time Series Data
#'
#'
#' @examples
#' # graph = msdGraph(x, year, firstStartDate="05-01", firstEndDate="08-31",
#' # secondStartDate ="06-01", secondEndDate="10-31", quantity = 2, window = 31)
#'
#' @export
#-----------------------------------------------------------------------------------------------------------------------------------------
msdGraph<-function(x, year, firstStartDate="05-01", firstEndDate="06-01", secondStartDate="08-31", secondEndDate="10-31", quantity=2, window=31, time=0){
#-----------------------------------------------------------------------------------------------------------------------------------------
#Calculate all stats for all years of the provided data, then select only the relevant year's data.
  allStats = msdMain(x, firstStartDate, firstEndDate, secondStartDate, secondEndDate, quantity, window, time)
  yearStats = subset(allStats, Years == year)
#-----------------------------------------------------------------------------------------------------------------------------------------
#Assemble the base timeseries to be plotted (if the input "x" is not already a timeseries)
  if (inherits(x, "timeseries")) {
    timeseriesFull = x
  } else if (inherits(x, "xts")) {
    timeseriesFull = x
  } else {
    datesSeq = NULL #!!!
    timeseriesFull = xts(x, datesSeq)
  }
#Subset the timeseries to only be the year (365 days) of interest
  allDates = seq(from = as.Date(time(timeseriesFull[1])), to = as.Date(time(timeseriesFull[length(timeseriesFull)])), by = 1)
  yearsOnly = format(allDates, "%Y")
  validDates = (year == yearsOnly)
  tsDates = allDates[validDates == TRUE]
#Filter the data for the purpose of graphing
  for(i in 1:quantity){
    timeseriesFull = apply(timeseriesFull, MARGIN = 2, FUN = msdFilter, window = window)
  }
  timeseriesPrecip = timeseriesFull[validDates == TRUE]
  timeseriesFrame = data.frame(tsDates, timeseriesPrecip)
  colnames(timeseriesFrame) = c("Date", "Precipitation")
#Create variables for the firstStart, secondStart, firstEnd, and secondEnd MM-DD inputs in YY-MM-DD format (add year of interest)
  startDate1 = as.Date(paste(year, firstStartDate, sep = "-"))
  endDate1 = as.Date(paste(year, firstEndDate, sep = "-"))
  startDate2 = as.Date(paste(year, secondStartDate, sep = "-"))
  endDate2 = as.Date(paste(year, secondEndDate, sep = "-"))

#-----------------------------------------------------------------------------------------------------------------------------------------
#Assign individual variables to each important column value from the yearStats dataframe (from msdMain)
  durationVal = yearStats[2]
  intensityVal = yearStats[3]
  firstMaxVal = yearStats[4]
  secondMaxVal = yearStats[6]
  minVal = yearStats[8]
#Create data frames of the dates and values corresponding to the maximums and minimums
  firstMax = data.frame(yearStats[5], firstMaxVal)
  colnames(firstMax) = c("Date", "Precipitation")
  secondMax = data.frame(yearStats[7], secondMaxVal)
  colnames(secondMax) = c("Date", "Precipitation")
  theMin = data.frame(yearStats[9], minVal)
  colnames(theMin) = c("Date", "Precipitation")

  durat<-function(x, y = 15){
    x[x<y]<-0
    x[x>y-1]<-1
    return(x)
  }
  durationX<-durat(durationVal)

  inten<-function(x, y = 3){
    x[x<y]<-0
    x[x>=y]<-1
    return(x)
  }
  intensityX<-inten(intensityVal)

  DIsum<-durationX + intensityX

  colors<-function(c){
    c[c!=2]<-"white"
    c[c==2]<-"lightblue"
    return(c)
  }
  color1<-colors(DIsum)
#-----------------------------------------------------------------------------------------------------------------------------------------
  output<-ggplot(data = timeseriesFrame, mapping = aes(x=Date,y=Precipitation))+theme_bw()+
    annotate("rect", xmin=timeseriesFrame[1,1], xmax=timeseriesFrame[length(tsDates),1], ymin=0, ymax=25, fill=color1[1], alpha=0.5)+
    geom_line(data=timeseriesFrame[1:nrow(timeseriesFrame),])+
    geom_point(data=firstMax, mapping = aes(x=as.Date(Date),y=Precipitation), size=2, color="red")+
    geom_point(data=secondMax, mapping = aes(x=as.Date(Date),y=Precipitation), size=2, color="red")+
    geom_point(data=theMin, mapping = aes(x=as.Date(Date),y=Precipitation), size=2, color="blue")+
    geom_vline(xintercept=startDate1, color="red")+ #!!! add year to these dates
    geom_vline(xintercept=endDate1, color="blue")+
    geom_vline(xintercept=startDate2, color="blue")+
    geom_vline(xintercept=endDate2, color="red")+
    xlab("Time (days)")+ylab("Precipitation (mm/day)")+
    scale_y_continuous(breaks = seq(0,25, by = 5), limits=c(0,25), expand = c(0,0))+
    theme(text = element_text(size=20))

  return(output)
}

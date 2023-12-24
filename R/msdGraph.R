#' @title Mid Summer Drought Time Series Graphs
#'
#' @description Plots the Time Series of Mid Summer Drought data
#'
#' @description The input must be in the form of daily data, with the first data point being January 1st of a respective year.
#'
#' @usage msdGraph(x, year, peakwindow1, minwindow2,
#' minwindow1, peakwindow2, quantity,
#' window, time)
#'
#' @param x                 Vector or TimeSeries
#' @param year              Year of interest
#' @param peakwindow1    desired date in MMDD format to begin analysis (window 1)
#' @param minwindow2      desired date in MMDD format to end analysis (window 1)
#' @param minwindow1   desired date in MMDD format to begin analysis (window 2)
#' @param peakwindow2     desired date in MMDD format to end analysis (window 2)
#' @param quantity          Amount of times the filter is run
#' @param window            Size of filter
#' @param time              Vector of dates (not needed for TimeSeries inputs)
#'
#' @return Graph of Time Series Data
#'
#'
#' @examples
#' # graph = msdGraph(x, year, peakwindow1="05-01", minwindow2="08-31",
#' # minwindow1 ="06-01", peakwindow2="10-31", quantity = 2, window = 31)
#'
#' @export
#-----------------------------------------------------------------------------------------------------------------------------------------
msdGraph<-function(x, year, peakwindow1 = "05-01", minwindow1 = "06-01", minwindow2 = "08-31", peakwindow2 = "10-31", quantity=2, window=31, time=0){
#-----------------------------------------------------------------------------------------------------------------------------------------
#Calculate all stats for all years of the provided data, then select only the relevant year's data.
  allStats = msdMain(x, peakwindow1, minwindow2, minwindow1, peakwindow2, quantity, window, time)
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
  timeseriesFull = apply(timeseriesFull, MARGIN = 2, FUN = msdFilter, window = window, quantity = quantity)
  timeseriesPrecip = timeseriesFull[validDates == TRUE]
  timeseriesFrame = data.frame(as.Date(tsDates), as.numeric(timeseriesPrecip))
  colnames(timeseriesFrame) = c("Date", "Precipitation")
#Create variables for the firstStart, secondStart, firstEnd, and secondEnd MM-DD inputs in YY-MM-DD format (add year of interest)
  pw1 = as.Date(paste(year, peakwindow1, sep = "-"))
  mw2 = as.Date(paste(year, minwindow2, sep = "-"))
  mw1 = as.Date(paste(year, minwindow1, sep = "-"))
  pw2 = as.Date(paste(year, peakwindow2, sep = "-"))

#-----------------------------------------------------------------------------------------------------------------------------------------
#Assign individual variables to each important column value from the yearStats dataframe (from msdMain)
  durationVal = as.numeric(yearStats[2])
  intensityVal = as.numeric(yearStats[3])
  firstMaxVal = as.numeric(yearStats[4])
  secondMaxVal = as.numeric(yearStats[6])
  minVal = as.numeric(yearStats[8])
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

  DIsum<- as.numeric(durationX) + as.numeric(intensityX)

  colors<-function(c){
    c[c!=2]<-"white"
    c[c==2]<-"lightblue"
    return(c)
  }
  color1<-colors(DIsum)
#-----------------------------------------------------------------------------------------------------------------------------------------
  output<-ggplot(data = timeseriesFrame, mapping = aes(x=Date,y=Precipitation))+theme_bw()+
    annotate("rect", xmin=timeseriesFrame[1,1], xmax=timeseriesFrame[length(tsDates),1], ymin=0, ymax=(max(firstMaxVal, secondMaxVal)+2), fill=color1[1], alpha=0.5)+
    geom_line(data=timeseriesFrame[1:nrow(timeseriesFrame),])+
    geom_point(data=firstMax, mapping = aes(x=as.Date(Date),y=Precipitation), size=2, color="red")+
    geom_point(data=secondMax, mapping = aes(x=as.Date(Date),y=Precipitation), size=2, color="red")+
    geom_point(data=theMin, mapping = aes(x=as.Date(Date),y=Precipitation), size=2, color="blue")+
    geom_vline(xintercept=pw1, color="red")+ #!!! add year to these dates
    geom_vline(xintercept=mw2, color="blue")+
    geom_vline(xintercept=mw1, color="blue")+
    geom_vline(xintercept=pw2, color="red")+
    xlab("")+
    ylab("Precipitation (mm/day)")+
    scale_y_continuous(breaks = seq(0,(max(firstMaxVal, secondMaxVal)+2), by = 5), limits=c(0,(max(firstMaxVal, secondMaxVal)+2)), expand = c(0,0))+
    theme(text = element_text(size=20))

  if (color1 == "white") {
    output = output + annotate("text", x=timeseriesFrame[30,1], y=max(firstMaxVal, secondMaxVal), label= "No MSD", size = 5)

  } else if (color1 == "lightblue") {
    output + annotate("text", x=timeseriesFrame[30,1], y=max(firstMaxVal, secondMaxVal), label= "MSD", size = 5)

  } else {
    print("Error! MSD neither existing nor nonexistent.")
  }

  return(output)
}

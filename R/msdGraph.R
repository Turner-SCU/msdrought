#' @title Mid Summer Drought Time Series Graphs
#'
#' @description Plots the Time Series of Mid Summer Drought data
#'
#' @description The input must be in the form of daily data, with the first data point being January 1st of a respective year.
#'
#' @usage msdGraph(x, y1, y2, y3, xlon, xlat)
#'
#' @param x          RasterBrick or TimeSeries
#' @param y1         Year 1
#' @param y2         Year 2
#' @param y3         Year 3
#' @param xlon       Longitude value
#' @param xlat       Latitude value
#'
#' @return Graph of Time Series Data
#'
#'
#' @examples
#' graph1 <- msdGraph(raster, 1981, 1985, 1986, 275, 15)
#'
#' @export

msdGraph<-function(x, y1, y2, y3, xlon, xlat){

  lower_bound<-function(p){
    p[p<5]<-1
    p[p>=5]<-NA
    return(p)
  }

  pts <- SpatialPoints(cbind(xlon,xlat), proj4string = CRS("+proj=longlat +ellps=WGS84 +towgs84"))

  points_data <- x %>%
    terra::extract(pts, df = TRUE) %>% #does not take negative lon
    gather(date, value, -ID) %>%
    spread(ID, value) %>%   # Can be skipped if you want a "long" table
    mutate(date = ymd(str_sub(names(x),2))) %>%
    as_tibble()
  colnames(points_data)[2] <- "pr"

  ts1<-filter31(points_data[,2])
  ts2<-filter31(ts1)

  ts4<-points_data[,1]
  ts4[,2]<-as.numeric(ts2)
  names(ts4)<-c("dates", "pr")
  ts4<-transform(ts4,pr=as.numeric(pr))
  year11<-x1-1980
  year12<-x2-1980
  year13<-x3-1980

  p1<-as.Date(paste(x1,"-01-01", sep=""))
  p2<-as.Date(paste(x1,"-12-31", sep=""))
  p3<-as.Date(paste(x2,"-01-01", sep=""))
  p4<-as.Date(paste(x2,"-12-31", sep=""))
  p5<-as.Date(paste(x3,"-01-01", sep=""))
  p6<-as.Date(paste(x3,"-12-31", sep=""))


  d1<-match(p1,ts4[,1])
  d2<-match(p2,ts4[,1])
  d3<-match(p3,ts4[,1])
  d4<-match(p4,ts4[,1])
  d5<-match(p5,ts4[,1])
  d6<-match(p6,ts4[,1])

  c1<-d2-d1
  c2<-d4-d3
  c3<-d6-d5

  if (c1==366){
    d2<-d2-1
  }

  if (c2==366){
    d4<-d4-1
  }

  if (c3==366){
    d6<-d6-1
  }

  pp1<-as.Date(paste(1981,"-01-01", sep=""))
  pp2<-as.Date(paste(1982,"-01-01", sep=""))
  pp3<-as.Date(paste(1983,"-01-01", sep=""))
  pp4<-as.Date(paste(1984,"-01-01", sep=""))

  dd1<-match(pp1,ts4[,1])
  dd2<-match(pp2,ts4[,1])
  dd3<-match(pp3,ts4[,1])
  dd4<-match(pp4,ts4[,1])

  ta<-ts4[c(d1:d2,d3:d4,d5:d6),]
  ta[,1]<-ts4[1:length(ta[,1]),1]

  trial1<-MSD(ta[,2])
  trial2<-MSD(ta[,2], fun="intensity")
  trial3<-MSD(ta[,2], fun="max1")
  trial4<-MSD(ta[,2], fun="max2")
  trial5<-MSD(ta[,2], fun="min")

  max11<-ta[match(trial3[1:3],ta[,2]),]
  max22<-ta[match(trial4[1:3],ta[,2]),]
  min11<-ta[match(trial5[1:3],ta[,2]),]

  dur11<-trial1[1:3]
  int11<-trial2[1:3]
  dur11<-dur(dur11)
  int11<-int(int11)
  sum111<-dur11+int11

  colors<-function(x){
    x[x!=2]<-"white"
    x[x==2]<-"lightblue"
    return(x)
  }
  color1<-colors(sum111)

  plot3<-ggplot(ta, aes(x=dates,y=pr))+theme_bw()+
    annotate("rect", xmin=ta$dates[dd1], xmax=ta$dates[dd2], ymin=0, ymax=25, fill=color1[1], alpha=0.5)+
    annotate("rect", xmin=ta$dates[dd2], xmax=ta$dates[dd3], ymin=0, ymax=25, fill=color1[2], alpha=0.5)+
    annotate("rect", xmin=ta$dates[dd3], xmax=ta$dates[dd4], ymin=0, ymax=25, fill=color1[3], alpha=0.5)+
    geom_line(data=ta[1:365,])+
    geom_line(data=ta[366:730,])+
    geom_line(data=ta[731:1095,])+
    geom_point(data=max11, size=2, color="red")+
    geom_point(data=max22, size=2, color="red")+
    geom_point(data=min11, size=2, color="blue")+
    geom_vline(xintercept=ta$dates[dated[1]], color="red")+
    geom_vline(xintercept=ta$dates[dated[5]], color="red")+
    geom_vline(xintercept=ta$dates[dated[9]], color="red")+
    geom_vline(xintercept=ta$dates[dated[4]], color="red")+
    geom_vline(xintercept=ta$dates[dated[8]], color="red")+
    geom_vline(xintercept=ta$dates[dated[12]], color="red")+
    geom_vline(xintercept=ta$dates[dated[2]], color="blue")+
    geom_vline(xintercept=ta$dates[dated[6]], color="blue")+
    geom_vline(xintercept=ta$dates[dated[10]], color="blue")+
    geom_vline(xintercept=ta$dates[dated[3]], color="blue")+
    geom_vline(xintercept=ta$dates[dated[7]], color="blue")+
    geom_vline(xintercept=ta$dates[dated[11]], color="blue")+
    scale_x_continuous(breaks = (c(pp1,pp2,pp3,pp4)),limits=(c(pp1,pp4)),expand=c(0,0), labels=c(p1,p3,p5,p6))+
    xlab("Time (days)")+ylab("Precipitation (mm/day)")+
    scale_y_continuous(breaks = seq(0,25, by = 5), limits=c(0,25), expand = c(0,0))+
    theme(text = element_text(size=20))

  return(plot3)
}

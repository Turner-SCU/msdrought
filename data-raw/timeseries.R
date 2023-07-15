# Steps to produce a timeseries (ts) of data for the MSD package
#
# Data origin: University of California, Santa Barbara Climate Hazards Group InfraRed Precipitation with Station data (CHIRPS)
# Daily precipitation data recorded from 1981 to 1985, trimmed to only highlight Nicaragua
#-------------------------------------------------------------------------------------------------------------------------------------------
library(terra)
library(dplyr)
library(usethis)
infile = rast('chirps.81-85.nc')
#-------------------------------------------------------------------------------------------------------------------------------------------
# Make a SpatRast for one point
lon = -86.2621555581 #El Bramadero lon (from NicaAgua)
lat = 13.3816217871 #El Bramadero lat (from NicaAgua)
lonLat = data.frame(lon=lon,lat=lat)

# Set up precipitation data
location = terra::vect(lonLat, crs = "+proj=longlat +datum=WGS84")
precip = terra::extract(infile, location, method = 'bilinear') %>%
  subset(select = -ID) %>%
  t()
precip[precip < 0] <- 0 #replace any negative (errant) values with zeroes
#------------------------------------------------------------------------------------------------------------------------------------------
# Set up dates (time) data
time = terra::time(infile) %>%
  as.Date()
# Make a TimeSeries for one point
timeseries = ts(data = precip, start = time[1], end = time[length(time)])
rownames(timeseries) <- as.character(time) #this makes the ts look nicer when printed
#usethis::use_data(timeseries, overwrite = TRUE) #export the data

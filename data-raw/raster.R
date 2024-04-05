# Steps to produce a spatraster of data for the MSDrought package
#
# Data origin: University of California, Santa Barbara Climate Hazards Group InfraRed Precipitation with Station data (CHIRPS)
# Daily precipitation data recorded from 1981 to 1985, trimmed to only highlight Nicaragua
#-------------------------------------------------------------------------------------------------------------------------------------------
library(terra)
library(usethis)
#-------------------------------------------------------------------------------------------------------------------------------------------
# Set up precipitation data
precip = rast('chirps.81-85.nc')
extent = terra::ext(-88, -70, 8, 20) #defined for the range of coordinates corresponding to Nicaragua
croppedData = terra::crop(precip, extent) #trim the data to only the data of interest
croppedData = terra::ifel(croppedData < 0, 0, croppedData) #replace any negative (errant) values with zeroes
raster = croppedData
#terra::plot(raster, range=c(0, 3000))
#usethis::use_data(raster, overwrite = TRUE) #save data externally

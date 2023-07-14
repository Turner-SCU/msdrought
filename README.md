<!-- README.md is generated from README.Rmd. Please edit that file -->



# MSD

<!-- badges: start -->

<!-- badges: end -->

The MSD R package contains functions for calculating characteristics of a mid-summer drought (MSD), a phenomenon of decreased rainfall during a typical rainy season.
The MSD is a feature of rainfall in much of Central America, but is also found in other locations, typically those with a Mediterranean climate.
The details are described in "The Mesoamerican mid-summer drought: the impact of its definition on occurrences and recent changes", HESS, Maurer et al. (2022) '<https://hess.copernicus.org/articles/26/1425/2022/>'.

## Installation

You can install the development version of MSD from [GitHub](https://github.com/Turner-SCU/MSD)

``` r
# install.packages("devtools")
devtools::install_github("Turner-SCU/MSD")
```

## Example

These are the steps required to create useful MSD data:

1)  **Begin with a timeseries or spatraster of precipitation data over time.**

    At the moment, the only applicable data is a vector of date and precipitation information.
    This will be changed.

2)  **Filter this data using the msdFilter function.**

    A bartlett noise filter is applied to the data in order to smooth the precipitation data over time via weighted average

3)  **Use the msdDates function to extract relevant dates for the calculations.**

    There are two sets of necessary dates when calculating the MSD statistics: the critical periods and the start and end of the year.
    This function determines both and stores them as a single vector.
    The following function breaks the two types of dates apart, so it is important to not change the output of the msdDates function before feeding it into the subsequent msd function.

4)  **Supply the msd function with precipitation data, the data from msdDates, and a selected parameter to calculate.**

    The precipitation data may be filtered using the msdFilter function.
    In order for this function to provide useful data, the input from msdDates must be unchanged from its original output.
    The following parameters are applicable to the MSD calculation: Duration, Intensity, firstMax, secondMax, min, and mindex.


```{r example}library(MSD)rainData = data_provided#Filter the infile data with a bartlett window of 31. Repeat 2xfilteredData = msdFilter(rainData, window = 31, quantity = 2)#Grab the appropriate dates, either from a file or by manually assigning themsomeDates <- seq(from = as.Date("1981-01-01"), to = as.Date("1985-12-31"), by = "day")dates = msdDates(someDates, firstStartDate="05-01",firstEndDate="08-31", secondStartDate ="06-01",secondEndDate="10-31")#Put everything all together!duration = msd(filteredData, dates, duration)intensity = msd(filteredData, dates, intensity)firstMax = msd(filteredData, dates, firstMax)secondMax = msd(filteredData, dates, secondMax)minimum = msd(filteredData, dates, min)mindex = msd(filteredData, dates, mindex)#You have now calculated the useful parameters for the Mid-Summer Drought!```
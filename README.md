
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MSD

<!-- badges: start -->
<!-- badges: end -->

The MSD R package contains functions for calculating characteristics of
a mid-summer drought (MSD), a phenomenon of decreased rainfall during a
typical rainy season. The MSD is a feature of rainfall in much of
Central America, but is also found in other locations, typically those
with a Mediterranean climate. The details are described in “The
Mesoamerican mid-summer drought: the impact of its definition on
occurrences and recent changes”, HESS, Maurer et al. (2022)
‘<https://hess.copernicus.org/articles/26/1425/2022/>’.

## Installation

You can install the development version of MSD from
[GitHub](https://github.com/Turner-SCU/MSD)

``` r
# install.packages("devtools")
devtools::install_github("Turner-SCU/MSD")
```

## Example

These are the steps required to create useful MSD data:

1.  Begin with a timeseries or spatraster of precipitation data over
    time.
2.  Filter this data using the msdFilter function.
3.  Use the msdDates function to extract relevant dates for the
    calculations.
4.  Supply the msd function with the data from msdDates and select a
    parameter to calculate.

``` r
library(MSD)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

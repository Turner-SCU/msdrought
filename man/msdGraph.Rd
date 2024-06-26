% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/msdGraph.R
\name{msdGraph}
\alias{msdGraph}
\title{Mid Summer Drought Time Series Graphs}
\usage{
msdGraph(x, year, peakwindow1, minwindow1,
minwindow2, peakwindow2, quantity,
window, timeVector)
}
\arguments{
\item{x}{vector of data or xts}

\item{year}{year of interest}

\item{peakwindow1}{date in MMDD format to begin analysis (window 1)}

\item{minwindow1}{date in MMDD format to end analysis (window 1)}

\item{minwindow2}{date in MMDD format to begin analysis (window 2)}

\item{peakwindow2}{date in MMDD format to end analysis (window 2)}

\item{quantity}{number of times the filter is to be run}

\item{window}{size of filter}

\item{timeVector}{vector of dates (not needed for xts inputs)}
}
\value{
Graph of Time Series Data
}
\description{
Plots the Time Series of Mid Summer Drought data. The input must be in the form of daily data,
with the first data point being January 1st of a respective year.
}
\examples{

\dontrun{
data("timeseries")
ts <- timeseries
msdrought::msdGraph(ts, 1982)
}

}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fiaDataFunctions.R
\name{getStartTimeStamp}
\alias{getStartTimeStamp}
\title{Get Start Time Stamp from an mzML file

A quick way to get this one information from a mzML file}
\usage{
getStartTimeStamp(xmlFile)
}
\arguments{
\item{xmlFile}{the mzML file to be read}
}
\value{
a list? object containing all startTimeStamp attributes from a mzML file
}
\description{
Get Start Time Stamp from an mzML file

A quick way to get this one information from a mzML file
}
\details{
Before loading an mzML file into xcms objects it can be beneficial
to get this time stamp in order to organize a large collection of
mzML files
}

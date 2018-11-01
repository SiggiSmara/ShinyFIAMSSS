% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fia_load.R
\name{convertOneWiffFolder}
\alias{convertOneWiffFolder}
\title{Call proteowizard msconvert to extract the data}
\usage{
convertOneWiffFolder(myTibbleRow)
}
\arguments{
\item{myTibbleRow}{a row from the findPotentialWiffDirs tibble that is returned}
}
\value{
Nothing is returned
}
\description{
Call proteowizard msconvert to extract the data
}
\details{
Calling the msconvert will create a folder in the working directory named by the date
and the barcode of the batch.
}
% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wiffConverting.R
\name{findPotentialWiffDirs}
\alias{findPotentialWiffDirs}
\title{Find wiff directories that may contain SS batches}
\usage{
findPotentialWiffDirs(parentPath, resPath, protwizPath,
  doConvert = FALSE, forceRecalc = FALSE)
}
\arguments{
\item{parentPath}{a file path that contains wiff data}

\item{resPath}{a file path that contains the resulting mzML files}

\item{protwizPath}{a file path to the proteowizard installation}

\item{doConvert}{boolean. If TRUE then msconvert will be called on
batches that are not present in the working directory.}

\item{forceRecalc}{boolean. If TRUE then all batches will be
reconverted with msconvert. This means that any results are also
lost.}
}
\value{
a tibble that contains all folders that potentially have full sets of SS
batches along with the information if that folder is found in the working directory
}
\description{
Find wiff directories that may contain SS batches
}
\details{
DONE: find all folders that have at least the spiked sample in them
DONE: find all wiff folders that have equal numbers of spikes and blanks
DONE: check if a barcode folder is present in resPath corresponding to a wiff folder
}

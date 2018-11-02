library(shiny)
library(DT)
library(tidyverse)
library(lubridate)
library(XML)
library(xcms)
library(MALDIquant)
library(shinyDirectoryInput)
library(R6)
#library(RColorBrewer)
#library(pander)
#library(doParallel)
#registerDoParallel(3)
#register(DoparParam(), default = TRUE)
#library(utils)



#' @name fiaR6
#' 
#' @title the FIA R6 object
#' 
#' @export 
fiaR6 <- R6Class("fiaR6",
  public = list(
    settings = NULL,
    myBiocFeatures = NULL, 
    FIAistds = NULL,
    myUIdata = NULL,
    debug = list(),
    initialize = function(workdirPath, wiffPath){
      initFiaR6(self, workdirPath, wiffPath)
      },
    runShinySettings = function(){ 
      runApp(getSettingsApp(self))
      },
    prepareForFIA = function(forceRecalc = FALSE) {
      prepForFIA(self, forceRecalc = forceRecalc)
    }
  )
)

#' @name initFiaR6
#' 
#' @title initialize the fiaR6 object
#' 
#' @param self passed from the pR6 object contains all public variables and functions
#' @param workdirPath the working directory path, contains both mzml data files, the results
#' and settings (if they are saved)
#' @param wiffPath the path to the topmost directory containing raw data files (to be converted via
#' proteowizard's msconvert)
#' 
#' @return invisible(self)
initFiaR6 <- function(self, workdirPath, wiffPath) {
  workdirPath <- normalizePath(workdirPath)
  wiffPath <- normalizePath(wiffPath)
  FIAspikesClean <- c('C0','C2','C3','C4','C5','C6 (C4:1-DC)','C8','C10','C12','C14','C16','C18','lysoPC a C18:0',
                     'PC aa C24:0','SM C18:0','H1','PC aa C36:0')
  self$settings <- list(fiaFile='FIA.csv', 
                          fiaIstdFile = 'FIA_ISTD.csv', 
                          fiaFeatures = FIAspikesClean,
                          convertWiffs = TRUE,
                          wiffPath = wiffPath,
                          workdirPath = workdirPath,
                          workdirRDataPath = file.path(workdirPath,'RData'),
                          workdirMZMLPath = file.path(workdirPath, 'MZML'),
                          protwizPath = 'c:/Program Files/ProteoWizard/ProteoWizard 3.0.18271.75bc4c4ea'
                    )
  invisible(self)
}


#' @name getSettingsApp
#' 
#' @title get the shiny app for the settings
#' 
#' @param self passed from the pR6 object contains all public variables and functions
#' 
#' @return shinyApp for the settings
#' 
getSettingsApp <- function(self) {
    return(shinyApp(ui = fiaShinySettingsUI(self), server = fiaShinySettingsServer(self)))
}

#' @name getFIAApp
#' 
#' @title Get the FIA MS SS app
#'
#' @param self passed from the pR6 object contains all public variables and functions
#'
#' @return shinyApp for the FIA SS checking
#'
getFIAApp <- function(self) {
  return(shinyApp(ui = fiaShinyUI(self), server = fiaShinyServer(self)))
}

#' @name prepForFIA
#' 
#' @title Prepare for FIA SS data exploration
#' 
#' @param self passed from the pR6 object contains all public variables and functions
#' 
#' @description 
#' First this function looks for any new wiff files to add to the results. Then it
#' tries to load the data from the converted files and put them into data objects
#' that can be used for data exploration
#' @return invisible(self)
prepForFIA <- function(self, forceRecalc = FALSE) {
  ##check for new datasets and convert them if they are found (based on a setting)
  allWiffPaths <- findPotentialWiffDirs(self$settings$wiffPath, 
                                        resPath = self$settings$workdirRDataPath, 
                                        protwizPath = self$settings$protwizPath,  
                                        doConvert=self$settings$convertWiffs)
  if(length(allWiffPaths) == 0) {
    warning("Wiff directory is not reachable!")
    msg <- paste('The wiff directory is not reachable!\n',
                 'Press any key to continue at your own risk\n',
                 'or else stop the script.')
    readline(msg)
  }
  
  ##TODO read the parent foldernames along with the other data
  ##map the foldernames to dates, format; begins with (ymd or dmy)
  ##if format does not fit make a mapping from name to date
  ##that can be saved and reused. Interface via shiny app.
  
  ##global objects used to assign the transitions
  #biocrates features
  self$myBiocFeatures <- read_tsv(file.path(self$settings$workdirPath,self$settings$fiaFile))
  #ISTDs with the cps limits (tested for in blank)
  self$FIAistds <- read_tsv(file.path(self$settings$workdirPath, self$settings$fiaIstdFile))
  
  loadFiaResults(self, forceRecalc=forceRecalc)
  
  ##Create the rest of the needed data objects to facilitate browsing
  self$myUIdata <- list()
  self$myUIdata$allDates <- self$resdataNice %>% group_by(batchDate) %>% summarise()
  self$myUIdata$allDates <-myUIdata$allDates$batchDate
  self$myUIdata$allYears <- unique(year(myUIdata$allDates))
  return(invisible(self))
}



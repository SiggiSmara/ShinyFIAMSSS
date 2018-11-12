# library(shiny)
# library(DT)
# library(tidyverse)
# library(lubridate)
# library(XML)
# library(xcms)
# library(MALDIquant)
# library(shinyDirectoryInput)
# library(R6)
# library(doParallel)

#library(RColorBrewer)
#library(pander)
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
    resdata = NULL,
    resdataNice = NULL,
    debug = list(),
    initialize = function(workdirPath, wiffPath = ''){
      initFiaR6(self, workdirPath, wiffPath)
      },
    runShinySettings = function(){
      runApp(getSettingsApp(self))
      },
    prepareForFIA = function(updateProgress = NULL) {
      prepForFIA(self, updateProgress)
    },
    runShinyFIA = function(){
      runApp(getFIAApp(self))
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
  convertWiffsetting <- FALSE
  workdirPath <- normalizePath(workdirPath)
  if(nchar(wiffPath)>0) {
    wiffPath <- normalizePath(wiffPath)
    convertWiffsetting <- TRUE
  }

  FIAspikesClean <- c('C0','C2','C3','C4','C5','C6 (C4:1-DC)','C8','C10','C12','C14','C16','C18','lysoPC a C18:0',
                     'PC aa C24:0','SM C18:0','H1','PC aa C36:0')
  self$settings <- list(fiaFile='features.csv',
                          fiaFeatures = FIAspikesClean,
                          convertWiffs = convertWiffsetting,
                          wiffPath = wiffPath,
                          workdirPath = workdirPath,
                          workdirRDataPath = file.path(workdirPath,'RData'),
                          workdirMZMLPath = file.path(workdirPath, 'MZML'),
                          protwizPath = 'c:/Program Files/ProteoWizard/ProteoWizard 3.0.18271.75bc4c4ea',
                          reloadData = FALSE,
                          forceRecalc = FALSE
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

#' @name fakeProgress
#'
#' @title Fake progress
#'
#' @param value double ranging from 0 to 1 indicating the progress
#' @param detail text for the progress bar
#'
#' @details
#' This is a fake progress function used when no progress function is passed.
#' The real progress function comes from shiny and is used to give the user
#' and indicator of the progress of a long process
fakeProgress <- function(value = NULL, detail = NULL) {
  print(paste(round(value,3), detail))
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
prepForFIA <- function(self, updateProgress = NULL) {

  if (!is.function(updateProgress)) {
    updateProgress = fakeProgress
  }
  updateProgress(value=0, detail = 'prepForFia started')
  ##check for new datasets and convert them if they are found (based on a setting)
  if(self$settings$convertWiffs) {
    updateProgress(value=1/4, detail = paste('Looking for new wiffs'))
    allWiffPaths <- findPotentialWiffDirs(self$settings$wiffPath,
                                          resPath = self$settings$workdirMZMLPath,
                                          protwizPath = self$settings$protwizPath,
                                          doConvert=self$settings$convertWiffs)
    if(length(allWiffPaths) == 0) {
      warning("Wiff directory is not reachable!")
      msg <- paste('The wiff directory is not reachable!\n',
                   'Press any key to continue at your own risk\n',
                   'or else stop the script.')
      readline(msg)
    }
    if(dim(allWiffPaths %>% filter(converted == FALSE))[1]>1) {
      #means there were files to convert, ensures the results are
      #calculated and loaded
      self$settings$reload <- TRUE
    }
  }

  ##TODO read the parent foldernames along with the other data
  ##map the foldernames to dates, format; begins with (ymd or dmy)
  ##if format does not fit make a mapping from name to date
  ##that can be saved and reused. Interface via shiny app.

  ##global objects used to assign the transitions
  updateProgress(value=2/4, detail = 'Loading results')
  self$myBiocFeatures <- read_csv(file.path(self$settings$workdirPath, self$settings$fiaFile), col_types = cols())
  self$myBiocFeatures <- mutate(self$myBiocFeatures, fName = as.factor(fName))
  loadFiaResults(self, updateProgress)

  ##Create the rest of the needed data objects to facilitate browsing
  updateProgress(value=3/4, detail = 'Preparing the final data')
  self$myUIdata <- list()
  #self$myUIdata$allDates <- self$resdataNice %>% group_by(batchDate) %>% summarise()
  #self$myUIdata$allDates <-self$myUIdata$allDates$batchDate

  self$myUIdata$allBatchNames <- self$resdataNice %>% arrange(desc(batchDate)) %>% select(batchName)
  self$myUIdata$allBatchNames <- unique(as.character(self$myUIdata$allBatchNames$batchName))

  self$myUIdata$allBatches <- unique(self$resdataNice %>%
                                       group_by(batchName, barcode, batchDate) %>%

                                       summarise()
                                     )
  self$myUIdata$ISTDs <- as.character(unlist(fiaSS$myBiocFeatures %>% filter(is_IS == 1) %>% select(fName), use.names = FALSE))
  updateProgress(value=4/4, detail = 'Done')
  return(invisible(self))
}



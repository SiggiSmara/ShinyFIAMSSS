library(shiny)
library(DT)
library(tidyverse)
library(lubridate)
library(XML)
library(xcms)
library(MALDIquant)
library(shinyDirectoryInput)
#library(RColorBrewer)
#library(pander)
#library(doParallel)
#registerDoParallel(3)
#register(DoparParam(), default = TRUE)
#library(utils)

# setting some global variables to NULL, will be assigned later to real objects / values
MZML_PATH <- NULL
FIAspikesClean <- NULL
settings <- NULL
globalSettings <- NULL
globalResdata <- NULL
globalResdataNice <- NULL
globalUIdata <- list()

#' @name runMainProgram
#' 
#' @title Run Main Program
#' 
#' @param WorkDirPath he path of the working directory that contains 
#' the mzML files, the FIA results and the settings for the data exploration.
#' 
#' @export
#' 
runMainProgram <- function(WorkDirPath = '') {
  
  # make sure that a path is supplied
  if(length(WorkDirPath) == 0) 
    stop('No working directory path supplied, please try again.')
  
  assign('MZML_PATH', normalizePath(WorkDirPath), inherits = TRUE)
  
  assign('FIAspikesClean' , c('C0','C2','C3','C4','C5','C6 (C4:1-DC)','C8','C10','C12','C14','C16','C18','lysoPC a C18:0',
                              'PC aa C24:0','SM C18:0','H1','PC aa C36:0'), inherits = TRUE)
  
  assign('settings', list(fiaFile='FIA.csv', 
                          fiaIstdFile = 'FIA_ISTD.csv', 
                          fiaFeatures = FIAspikesClean,
                          mzmlPath = MZML_PATH,
                          wiffPath = '/media/ssmarason/qtrap/Analyst Data/Projects/CHRIS_Biocrates',
                          convertWiffs = TRUE), 
         inherits=TRUE)
  
  ##run the settings shiny app. The end result is that the settings
  ##are saved into the globalSettings object
  appDir <- system.file("assets", "set_settings", package = "ShinyFIAMSSS")
  runApp(appDir)
  
  #source('/home/ssmarason/ownCloud/metidq_data/shiny/fia_shiny/fia_load.R')
  
  ##check for new datasets and convert them if they are found (based on a setting)
  allWiffPaths <- findPotentialWiffDirs(globalSettings$wiffPath, doConvert=globalSettings$convertWiffs)
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
  myBiocFeatures <- read_tsv(file.path(MZML_PATH,globalSettings$fiaFile))
  #ISTDs with the cps limits (tested for in blank)
  FIAistds <- read_tsv(file.path(MZML_PATH,globalSettings$fiaIstdFile))
  loadFiaResults()
  
  ##Create the rest of the needed data objects to facilitate browsing
  myUIdata <- get('globalUIdata')
  myUIdata$allDates <- globalResdataNice %>% group_by(batchDate) %>% summarise()
  myUIdata$allDates <-myUIdata$allDates$batchDate
  myUIdata$allYears <- unique(year(myUIdata$allDates))
  assign('globalUIdata', myUIdata, inherits=TRUE)
  
  ##run the data exploration shiny app
  appDir <- system.file("assets", "assess_fia", package = "ShinyFIAMSSS")
  runApp(appDir)
}

#runMainProgram('/home/ssmarason/ownCloud/FIA_SS_data')





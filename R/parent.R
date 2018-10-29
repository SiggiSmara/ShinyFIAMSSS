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
#' @param WorkDirPath a path to the working directory
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
  
  assign('settings', list(fiaFile='FIA.csv', fiaIstdFile = 'FIA_ISTD.csv', fiaFeatures = FIAspikesClean), inherits=TRUE)
  
  runApp(file.path(MZML_PATH,'/shiny/fia_settings'))
  
  #biocrates features
  myBiocFeatures <- NULL
  
  #ISTDs with the cps limits (tested for in blank)
  FIAistds <- NULL
  
  source(file.path(MZML_PATH,'shiny', 'fia_shiny', 'fia_load.R'))
  
  wiffPath <- '/media/ssmarason/qtrap/Analyst Data/Projects/CHRIS_Biocrates'
  allWiffPaths <- findPotentialWiffDirs(wiffPath, doConvert=TRUE)
  if(length(allWiffPaths) == 0) {
    warning("Wiff directory is not reachable!")
    msg <- paste('The wiff directory is not reachable!\n',
                 'Press any key to continue at your own risk\n',
                 'or else stop the script.')
    readline(msg)
  }
  
  runApp(file.path(MZML_PATH,'/shiny/fia_shiny'))
}





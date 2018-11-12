# library(tidyverse)
# library(lubridate)
# library(XML)
# library(xcms)
# library(MALDIquant)
# library(RColorBrewer)
# library(pander)
#library(doParallel)
#registerDoParallel(3)
#register(DoparParam(), default = TRUE)

# #biocrates features
# myBiocFeatures <- read_tsv(file.path(MZML_PATH,globalSettings$fiaFile))
#
# #ISTDs with the cps limits (tested for in blank)
# FIAistds <- read_tsv(file.path(MZML_PATH,globalSettings$fiaIstdFile))

#' @name getStartTimeStamp
#'
#' @title Get Start Time Stamp from an mzML file
#'
#' A quick way to get this one information from a mzML file
#'
#' @param xmlFile  the mzML file to be read
#'
#' @details
#' Before loading an mzML file into xcms objects it can be beneficial
#' to get this time stamp in order to organize a large collection of
#' mzML files
#'
#' @return a list? object containing all startTimeStamp attributes from a mzML file
getStartTimeStamp <- function(xmlFile) {
  if(file.exists(xmlFile)) {
    test <- xmlParse(xmlFile)
    xmltop <- xmlRoot(test)
    retVals <- c()
    for(i in 1:xmlSize(xmltop)) {
      if(xmlName(xmltop[[i]]) == "mzML") {
        for(j in 1:xmlSize(xmltop[[i]])) {
          if(xmlName(xmltop[[i]][[j]]) == "run") {
            retVals <- rbind(retVals, xmlGetAttr(xmltop[[i]][[j]],"startTimeStamp"))
          }
        }
      }
    }
  } else {
    retVals <- c('0000-00-00 00:00:00')
  }
  return(retVals)
}


#' @name parseFilenames
#'
#' @title Split a file name by '.' or '_' or '-'
#'
#' @param fileTibble a tibble that contains file names in a column called sName
#' @param splitcols a character vector containing the column names of the split. Any
#' column whose name starts with 'XX' will be discarded before returning the tible
#' @param sortcols a character vector containing the columns to be used for sorting and
#' comparison for duplicate runs (basically different batches but same file name)
#'
#' @details
#' Every filename and splits it by one of three characters ('.' or '_' or '-').
#'
#' @return a tibble with strings split from the file name and two more columns added
#' one indicating the run time of the file and the other indicating the batch number
#' if the information in the tibble is otherwise identical
#'
# barcode, well, polarity, extra1, extra2, sample type, sample name
parseFilenames <- function(fileTibble, splitcols, sortcols) {
  #retVals <- unlist(str_split(oneName,"[._-]"))[retcols]
  #names(retVals) <- names(retcols)
  fileTibble <- fileTibble %>% mutate(fileName = basename(sName))
  fileTibble <- fileTibble %>% separate(fileName, into=splitcols, sep="[._-]", remove=FALSE)

  fileTibble <- fileTibble %>% select(sortcols, everything())
  fileTibble <- parseDups(fileTibble, testCols=sortcols)

  #convert the tStamp to a date and add more columns
  #fileTibble <- fileTibble %>% mutate(tStamp = unlist(lapply(fileTibble$sName, getStartTimeStamp)))

  #return the tibble
  selcols <- names(fileTibble)
  selcols <- selcols[str_sub(selcols,1,2)!='XX']
  return(fileTibble %>% select(selcols))
}

#' @name parseDups
#'
#' @title parse out diplicates from a tibble
#'
#' @param fileTibble a tibble
#' @param testCols a string vector with column names to use
#' for determining if there are more than one batch.
#'
#' @details
#' parsing a number of filenames where sometimes the same
#' file name is generated at different times (different batches)
#' This function adds another column (batchNo) which is a running
#' number that indicates different batches with the same file name.
#'
#' @return
#' A tibble with a new colum batchNo indicating the batch number
# look for repeats and give them a different batch number
parseDups <- function(fileTibble, testCols) {
  testdata <- fileTibble %>% mutate(batchNo = -1) %>%
    arrange_all()

  testdata[1,"batchNo"] <- 1
  for(i in 2:dim(testdata)[1]) {
    if(all(testdata[i, testCols] == testdata[(i-1), testCols])) {
      testdata[i,"batchNo"] <- testdata[(i-1),"batchNo"] + 1
    } else {
      testdata[i,"batchNo"] <- 1
    }
  }
  return(testdata)
}

#' @name processOneMZML
#'
#' @title Process one mzML file
#'
#' @description
#' Read one mzML file, calculate mean values for each transition and write the
#' result into a tsv table with the same name as the mzML file
#'
#' @param filePaht  the path (including the filename) of the mzML file
#'
#' @return Nothing
processOneMZML <- function(filePath) {
  myOrigData <- readSRMData(filePath)
  sampleNames(myOrigData) <- c(filePath)
  mzRfeatures <- as.tibble(as(featureData(myOrigData), "data.frame"))
  mzRfeatures <- mzRfeatures %>% mutate(Q1 = precursorIsolationWindowTargetMZ,
                                        Q3 = productIsolationWindowTargetMZ)
  mzRfeatures <- mzRfeatures %>% unite(fName, Q1, Q3, polarity)
  featureNames(myOrigData) <- mzRfeatures$fName
  myData <- calculateMeanValues(myOrigData)
  myData$tStamp <- rep(getStartTimeStamp(filePath), dim(myData)[1])
  myResultFile <- paste0(str_sub(filePath, 1, nchar(filePath)-4),'tsv')
  if(file.exists(myResultFile)) {
    unlink(myResultFile)
  }
  write_tsv(myData, myResultFile)
}


#' @name  processOneFolder
#'
#' @title Process one mzML folder
#'
#' @description
#' Use multi-core processing (if set-up) to read mzML files from one directory
#' and write out the results for each file
#'
#' @param folderPath the path to the mzML folder
processOneFolder <- function(folderPath, resultName, forceRecalc = FALSE) {
  fls <- list.files(folderPath,'*.mzML', recursive=FALSE)
  if(length(fls)>0) {
    #print("starting bplapply")
    if(forceRecalc) {
      bplapply(file.path(folderPath,fls), processOneMZML)
    }
    #print("bplapply finished")
    resTibble <- NULL
    for(oneFile in fls) {
      myResName <- paste0(str_sub(oneFile,1, nchar(oneFile)-4),'tsv')
      myResName <- file.path(folderPath,myResName)
      resTibble <- bind_rows(resTibble, read_tsv(myResName, col_types = cols()))
    }
    resFilePath <- file.path(folderPath, resultName)
    if(file.exists(resFilePath)) {
      unlink(resFilePath)
    }
    write.table(resTibble, resFilePath, row.names = FALSE, sep = '\t')
  } else{
    print("no mzML files found")
  }
}

#' @name calculateMeanValues
#'
#' @title Calculate the mean intensity for each trace and return it in a matrix format
#'
#' @details
#' The core function of this package is to calculate a single value for each FIA trace.
#' That core function is performed here.
#'
#' @param fiaExp  the xcmsExp object for one fia batch
#'
#' @return a tibble with three columns, fName, sName and fiaValue.
#' fName is the feature (mrm) name, sName is the sample name and fiaValue
#' is the the fia cps value for a particular sample and feature.
#calculate the mean intensity for each trace and return it in a matrix format
calculateMeanValues <- function(fiaExp) {
  fnams <- featureNames(fiaExp)
  samplnams <- sampleNames(fiaExp)
  resTibble <- NULL
  #tibble(fName = character(), sName=character, fiaValue = numeric())
  #colnames(resMatrix) <- samplnams
  #rownames(resMatrix) <- fnams
  for(oneFeatIdx in 1:length(fnams)) {
    oneRes <- rep(0.0, length(samplnams))
    for(oneSamplIdx in 1:length(samplnams)) {
      mydata <- fiaExp[oneFeatIdx,oneSamplIdx]@intensity
      if(length(mydata) > 3) {
        mydata <- MALDIquant:::.movingAverage(mydata, halfWindowSize = 1L, weighted=TRUE)
        oneRes[oneSamplIdx] <- base::mean(mydata[1:20], na.rm=TRUE)
      } else {
        oneRes[oneSamplIdx] <- NA
      }
    }
    test <- tibble(
      fName=rep(fnams[oneFeatIdx], length(samplnams)),
      sName=samplnams,
      fiaValue = oneRes
    )
    resTibble <- bind_rows(resTibble, test)
  }
  return(resTibble)
}

#' @name readOneFolder
#'
#' @title Read results from a folder containing data files from one experiment
#'
#' @param onePath a path to a batch of SS files generated
#' @param resultName the name of the file that is used to save the result
#' @param forceRecalc boolean. If TRUE then all mzML files are reprocessed.
#' If FALSE then the already generated results are looked for first and returned
#' if found.
#'
#' @details
#'
#' @return a tibble of results for all experiments found in the folder or
#' an emtpy tibble if no results should be returned
#'
# test for a result table, if not found then
# generate one, otherwise just load the result table
# forceRecalc = TRUE skips the test and recalculates the
# result.
readOneFolder <- function(self, onePath, resultName = 'result.tsv', forceRecalc = FALSE) {
  oneResdata <- tibble()
  resFilePath <- file.path(onePath, resultName)
  if(!file.exists(resFilePath) || forceRecalc ) {
    #print(paste("recalculating...", onePath))
    if(file.exists(resFilePath)) {
      unlink(resFilePath)
    }
    processOneFolder(onePath, resultName, forceRecalc)
  }
  if(file.exists(resFilePath)) {
    oneResdata <- read_tsv(resFilePath, col_types = cols())
  }
  return(oneResdata)
}


#' @name reloadFiaResults
#'
#' @title reload the fia results from the working directory
#'
#' @param forceRecalc boolean. If TRUE then mzML files are read
#' again and the results generated again.
#'
#' @details
#' Loads the results from each batch folder
#' and combine them into two tibbles which are then saved in two
#' RData objects that are used overall in the package. forceRecalc = TRUE
#' will result in loading the mzML files and re-assessing the FIA value
#' for all transitions.
#'
#' @return nothing
reloadFiaResults <- function(self, updateProgress, forceRecalc = FALSE) {
  #read the mzML files and the cps data from biocrates
  if(is.function(updateProgress)) {
    updateProgress = fakeProgress
  }
  resPath <- self$settings$workdirMZMLPath
  datafolders <- list.dirs(resPath)
  datafolders <- datafolders[grep("[0-9]{7}",basename(datafolders))]
  resdata <- NULL

  updateProgress(value=0, detail = paste('Reloading data with forceRecalc =', forceRecalc))

  for(i in 1:length(datafolders)) {
    fpath <- datafolders[i]
    resdata  <- bind_rows(resdata,readOneFolder(self, fpath, forceRecalc = forceRecalc))

    updateProgress(value=i/length(datafolders),
                   detail = paste('Reloading data with forceRecalc =', forceRecalc))
  }

  #asign feature names and test information

  updateProgress(value=0, detail = 'Reloading data - reformatting')

  resdata <- resdata %>% separate(fName, into=c('Q1','Q3','polarity'), sep='_') %>%
    mutate(Q1 = as.numeric(Q1), Q3 = as.numeric(Q3), polarity=as.integer(polarity)) %>%
    inner_join(self$myBiocFeatures)

  #parse the sName into something more useful
  retcols <- vector("character", 16)
  for(h in 1:16) {
    retcols[[h]] <- paste('XX',h)
  }
  retcols[[4]] <- "barcode"
  retcols[[5]] <- "well"
  retcols[[6]] <- "runNo"
  retcols[[7]] <- "extra1"
  retcols[[8]] <- "extra2"
  retcols[[9]] <- "sampleType"
  retcols[[15]] <- "sampleName"
  sortcols <- c("barcode","well","runNo","extra1","extra2","sampleType", "sampleName", "sName")

  updateProgress(value=1/5, detail = 'Reloading data - reformatting')
  fileNames <- as.tibble(unique(resdata$sName))
  names(fileNames) <- c("sName")
  fileNames <- parseFilenames(fileNames, retcols, sortcols)

  fileNames <- fileNames %>% mutate(
    batchName = as.factor(basename(dirname(dirname(sName)))),
    well = as.factor(well),
    runNo = as.factor(runNo),
    sampleType = as.factor(sampleType),
    sampleName = as.factor(sampleName)
  )
  resdata <- resdata %>% inner_join(fileNames)

  #add more columns
  updateProgress(value=2/5, detail = 'Reloading data - reformatting')
  resdata <- mutate(resdata,
                    fName = as.factor(fName),
                    included = 1,
                    tStamp = ymd_hms(tStamp),
                    sampleTypeName = as.factor(ifelse(sampleType =='01','Blank','SS'))
   )

  #exclude some known suspects... TODO: move this to a separate
  #RData object to be saved in the workdir
  #resdata$included[resdata$batchNo>1] = 0
  resdata$included[is.na(resdata$fiaValue)] = 0
  resdata$included[resdata$barcode =='1015693595'] = 0
  resdata$included[resdata$barcode =='1026741254'] = 0
  resdata$included[resdata$barcode =='1026743063'] = 0
  resdata$included[resdata$barcode =='1015705381' & resdata$well == '06'] = 0
  resdata$included[resdata$barcode =='1015709562' & resdata$well == '68'] = 0
  resdata$included[resdata$barcode =='1023158371' & resdata$well == '02'] = 0
  resdata$included[resdata$barcode =='1026743112' & resdata$well == '14'] = 0
  resdata$included[resdata$barcode =='1026743112' & resdata$well == '02'] = 0

  resdataNice <- resdata %>%
    filter(included == 1 ) %>%
    group_by(fName, sampleTypeName, polarity) %>%
    mutate(grpMedVal = median(fiaValue),
           fiaValueRLA = fiaValue/grpMedVal
           ) %>%
    ungroup()

  updateProgress(value=3/5, detail = 'Reloading data - reformatting')

  resdataNice <- unite(resdataNice, 'type_pol', c('sampleTypeName', 'polarity'), remove = FALSE)

  resdataNice <- resdataNice %>%
    group_by(fName, barcode, batchNo) %>%
    mutate(batchDate = min(tStamp),
           barc_batch_bname = paste(batchName,
                                   batchNo,
                                    barcode,
                                   sep='_')) %>%
    ungroup()

  updateProgress(value=4/5, detail = 'Reloading data - reformatting')

  resdataNice <- resdataNice %>%
    #group_by(fName, sampleTypeName, type_pol, barc_batch_bname) %>%
    arrange(tStamp)
  updateProgress(value=5/5, detail = 'Reloading data - reformatting')

  if(!dir.exists(self$settings$workdirRDataPath)) {
    dir.create(self$settings$workdirRDataPath, recursive = TRUE)
  }
  if(file.exists(file.path(self$settings$workdirRDataPath, 'resdata.RData'))) {
    unlink(file.path(self$settings$workdirRDataPath, 'resdata.RData'))
  }
  if(file.exists(file.path(self$settings$workdirRDataPath, 'resdataNice.RData'))) {
    unlink(file.path(self$settings$workdirRDataPath, 'resdataNice.RData'))
  }
  save(resdata, file=file.path(self$settings$workdirRDataPath, 'resdata.RData'))
  save(resdataNice, file=file.path(self$settings$workdirRDataPath, 'resdataNice.RData'))

}


#' @name loadFiaResults
#'
#' @title Load the FIA results
#'
#' @param self passed from the pR6 object contains all public variables and functions
#' @param reload Boolean. Should the data be refreshed from result files. Will cause
#' \code{\link{reloadFiaResults}} to be called if TRUE. Will reload the result files
#' generated from \code{\link{processOneFolder}}
#' @param forceRecalc Boolean. Will cause \code{\link{reloadFiaResults}} to be
#' called if TRUE. Will also force the recalculation of the results.
#'
#' @details
#' Load the FIA resutls, passes the forceRecalc variable to the underlying
#' reloadFiaResults function that is called if the RData files are not found
#'
#' @return no return value, assigns to self$resdata
#' and self$resdataNice the loaded values
#'
loadFiaResults <- function(self, updateProgress) {
  reloadData <- self$settings$reloadData
  forceRecalc <- self$settings$forceRecalc
  if(reloadData || forceRecalc) {
    reloadFiaResults(self, forceRecalc = forceRecalc, updateProgress = updateProgress)
    load(file.path(self$settings$workdirRDataPath, 'resdata.RData'))
    load(file.path(self$settings$workdirRDataPath, 'resdataNice.RData'))
  } else {
    loaded <- FALSE
    if(file.exists(file.path(self$settings$workdirRDataPath, 'resdata.RData'))) {
      load(file.path(self$settings$workdirRDataPath, 'resdata.RData'))
      loaded <- TRUE
    }
    if(file.exists(file.path(self$settings$workdirRDataPath, 'resdataNice.RData'))) {
      load(file.path(self$settings$workdirRDataPath, 'resdataNice.RData'))
      loaded <- TRUE & loaded
    }

    if(!loaded) {
      reloadFiaResults(self, forceRecalc = forceRecalc, updateProgress = updateProgress)
      load(file.path(self$settings$workdirRDataPath, 'resdata.RData'))
      load(file.path(self$settings$workdirRDataPath, 'resdataNice.RData'))
    }
  }

  self$resdata <- resdata
  updateProgress(value=0.5, detail ='Loading preprocessed data...')

  self$resdataNice <- resdataNice
  updateProgress(value=1, detail ='Loading preprocessed data...')
}



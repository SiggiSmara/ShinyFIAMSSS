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
  fileTibble <- fileTibble %>% mutate(tStamp = unlist(lapply(fileTibble$sName, getStartTimeStamp)))

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
  # sampleNames(myOrigData) <- c(filePath)
  # mzRfeatures <- as.tibble(as(featureData(myOrigData), "data.frame"))
  # mzRfeatures <- mzRfeatures %>% mutate(Q1 = precursorIsolationWindowTargetMZ,
  #                                       Q3 = productIsolationWindowTargetMZ)
  # mzRfeatures <- mzRfeatures %>% unite(fName, Q1, Q3, polarity)
  # featureNames(myOrigData) <- mzRfeatures$fName
  # myData <- calculateMeanValues(myOrigData)
  # myResultFile <- paste0(str_sub(filePath, 1, nchar(filePath)-4),'tsv')
  # if(file.exists(myResultFile)) {
  #   unlink(myResultFile)
  # }
  # write_tsv(myData, myResultFile)
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
processOneFolder <- function(folderPath, resultName) {
  fls <- list.files(folderPath,'*.mzML', recursive=FALSE)
  if(length(fls)>0) {
    #print("starting bplapply")
    bplapply(file.path(folderPath,fls), processOneMZML)
    #print("bplapply finished")
    resTibble <- NULL
    for(oneFile in fls) {
      myResName <- paste0(str_sub(oneFile,1, nchar(oneFile)-4),'tsv')
      myResName <- file.path(folderPath,myResName)
      resTibble <- bind_rows(resTibble, read_tsv(myResName, col_types = cols()))
    }
    resFilePath <- file.path(folderPath, resultName)
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
    print(paste("recalculating...", onePath))
    if(file.exists(resFilePath)) {
      unlink(resFilePath)
    }
    processOneFolder(onePath, resultName)
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
reloadFiaResults <- function(self, forceRecalc = FALSE) {
  #read the mzML files and the cps data from biocrates
  resPath <- self$settings$workdirMZMLPath
  datafolders <- list.dirs(resPath)
  datafolders <- datafolders[grep("[0-9]{7}",basename(datafolders))]
  resdata <- NULL
  for(fpath in datafolders) {
    oneFolder <- readOneFolder(self, fpath, forceRecalc = forceRecalc)
    if(dim(oneFolder)[1] >0) {
      resdata  <- bind_rows(resdata,readOneFolder(self, fpath, forceRecalc = forceRecalc))
    }
  }

  #asign feature names and test information
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
  resdata <- mutate(resdata, included = 1,
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

  resdataNice <- unite(resdataNice, 'type_pol', c('sampleTypeName', 'polarity'), remove = FALSE)

  resdataNice <- resdataNice %>%
    group_by(fName, barcode, batchNo) %>%
    mutate(batchDate = min(tStamp),
           barc_batch_bname = paste(batchName,
                                   batchNo,
                                    barcode,
                                   sep='_')) %>%
    ungroup()

  resdataNice <- resdataNice %>%
    group_by(fName, sampleTypeName, type_pol, barc_batch_bname) %>%
    arrange(barcode, batchNo, tStamp)

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
loadFiaResults <- function(self) {
  reloadData <- self$settings$reloadData
  forceRecalc <- self$settings$forceRecalc
  if(reloadData || forceRecalc) {
    reloadFiaResults(self, forceRecalc = forceRecalc)
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
      reloadFiaResults(self, forceRecalc = forceRecalc)
      load(file.path(self$settings$workdirRDataPath, 'resdata.RData'))
      load(file.path(self$settings$workdirRDataPath, 'resdataNice.RData'))
    }
  }

  self$resdata <- resdata
  self$resdataNice <- resdataNice
}


#' #' @name findPotentialWiffDirs
#' #'
#' #' @title Find wiff directories that may contain SS batches
#' #'
#' #' @param parentPath a file path that contains wiff data
#' #' @param resPath a file path that contains the resulting mzML files
#' #' @param protwizPath a file path to the proteowizard installation
#' #' @param doConvert boolean. If TRUE then msconvert will be called on
#' #' batches that are not present in the working directory.
#' #' @param forceRecalc boolean. If TRUE then all batches will be
#' #' reconverted with msconvert. This means that any results are also
#' #' lost.
#' #'
#' #' @details
#' #'
#' #' DONE: find all folders that have at least the spiked sample in them
#' #' DONE: find all wiff folders that have equal numbers of spikes and blanks
#' #' DONE: check if a barcode folder is present in resPath corresponding to a wiff folder
#' #'
#' #' @return a tibble that contains all folders that potentially have full sets of SS
#' #' batches along with the information if that folder is found in the working directory
#' findPotentialWiffDirs <- function(parentPath, resPath, protwizPath, doConvert = FALSE, forceRecalc = FALSE) {
#'   if(!dir.exists(parentPath)) return()
#'   ss_spike_name <- "20000004\\.wiff"
#'   blank_name <-  "10000001\\.wiff"
#'
#'   ss_namelen <- nchar(ss_spike_name)
#'   blank_nameLen <- nchar(blank_name)
#'   prec_len <- nchar(parentPath)+1
#'
#'   foundFiles = list.files(parentPath, pattern = ss_spike_name, recursive = TRUE)
#'   for(i in 1:length(foundFiles)) {
#'     foundFiles[i] <- dirname(foundFiles[i])
#'   }
#'   foundFiles <- unique(foundFiles)
#'   goodFolders <- rep('',length(foundFiles))
#'
#'   res <- tibble(folder = character(), barcode = character(), converted = logical(), fullPath=character())
#'   for(i in 1:length(goodFolders)) {
#'     spikeFounds <- list.files(file.path(parentPath,foundFiles[i]), pattern = ss_spike_name)
#'     blankFounds <- list.files(file.path(parentPath,foundFiles[i]), pattern = blank_name)
#'     if(length(spikeFounds) == length(blankFounds)) {
#'       folder = basename(foundFiles[i])
#'       if(str_detect(folder,'FIA')) {
#'         #print(folder)
#'         folder <- basename(dirname(file.path(parentPath,foundFiles[i])))
#'         #print(folder)
#'       }
#'       barcodeVector <- unique(unlist(lapply(str_split(spikeFounds,'_'), FUN = function(x) {x[2]})))
#'       for(barcode in barcodeVector) {
#'         converted <- dir.exists(file.path(resPath, folder, barcode))
#'         res <- bind_rows(res, tibble( folder = folder,
#'                                       barcode = barcode,
#'                                       converted = converted,
#'                                       fullPath = file.path(parentPath,foundFiles[i])
#'         )
#'         )
#'       }
#'     }
#'   }
#'   if(doConvert) {
#'     unconverts <- res %>% filter(converted == FALSE)
#'     if(forceRecalc) {
#'       unconverts <- res
#'     }
#'     if(dim(unconverts)[1]>0){
#'       apply(unconverts, MARGIN=1, FUN=convertOneWiffFolder, resPath = resPath, protwizPath = protwizPath)
#'     }
#'   }
#'   return(res)
#' }
#'
#'
#'
#' #' @name convertOneWiffFolder
#' #'
#' #' @title Call proteowizard msconvert to extract the data
#' #'
#' #' @param myTibbleRow a row from the findPotentialWiffDirs tibble that is returned
#' #'
#' #' @details
#' #' Calling the msconvert will create a folder in the working directory named by the date
#' #' and the barcode of the batch.
#' #'
#' #' @return Nothing is returned
#' #parPath <- '/media/ssmarason/qtrap/Analyst Data/Projects/CHRIS_Biocrates'
#' #allWiffPaths <- findPotentialWiffDirs(parPath, doConvert=TRUE)
#' #allWiffPaths %>% filter(converted == FALSE)
#' # DONE: call the proteowizard msconvert to convert wiff files to mzML files and put them in the right barcode folder
#' convertOneWiffFolder <- function(myTibbleRow, resPath, protwizPath) {
#'
#'   wiffPath <- myTibbleRow['fullPath']
#'   destPath <- file.path(resPath, myTibbleRow['folder'], myTibbleRow['barcode'])
#'   unlink(destPath, recursive = TRUE)
#'   dir.create(destPath, recursive = TRUE)
#'   if(.Platform$OS.type == 'windows'){
#'     command <- sprintf('"%s"',normalizePath(paste0(protwizPath,"/msconvert")))
#'     args <- paste(sprintf('"%s"',file.path(wiffPath,paste0('KIT*_',myTibbleRow['barcode'],'*.wiff'))),
#'                   '-z',
#'                   '-o',
#'                   sprintf('"%s"',destPath)
#'     )
#'   } else {
#'     command <- 'wine'
#'     args <- paste(sprintf('"%s"',normalizePath(paste0(protwizPath,"/msconvert"))),
#'                   sprintf('"%s"',file.path(wiffPath,paste0('KIT*_',myTibbleRow['barcode'],'*.wiff'))),
#'                   '-z',
#'                   '-o',
#'                   sprintf('"%s"',destPath)
#'     )
#'   }
#'   suppressWarnings({
#'     system2(command, args)
#'   })
#' }

# datafolders <- paste0(MZML_PATH, c('/1023162039','/1023161974'))
# resdata <- NULL
# for(i in 1:length(datafolders)) {
#   fpath <- datafolders[i]
#   print(fpath)
#   resdata  <- bind_rows(resdata,readOneFolder(fpath, forceRecalc = TRUE))
#
# }

# ## old way
# datafolders <- paste0(MZML_PATH, c('/1023162039','/1023161974'))
# alldata <- vector("list", 2)
# for(i in 1:length(datafolders)) {
#   fpath <- datafolders[i] #file.path(MZML_PATH,datafolders[i])
#   print(fpath)
#   alldata[[i]] <- readFiadata(fpath)
# }
# resdata <- NULL
# for(i in 1:length(datafolders)) {
#   for(onePol in alldata[[i]]) {
#     if(length(onePol)>0) {
#       resdata  <- bind_rows(resdata,calculateMeanValues(onePol))
#     }
#   }
# }
# #View(alldata[[i]])
# '13C6-Gluc' %in% featureNames(alldata[[2]][[1]])
# '13C6-Gluc' %in% featureNames(alldata[[2]][[2]])
# '13C6-Gluc' %in% featureNames(alldata[[1]][[1]])
# '13C6-Gluc' %in% featureNames(alldata[[1]][[2]])
#
# 'H1_13C6_204_186' %in% featureNames(alldata[[2]][[1]])
# 'H1_13C6_204_186' %in% featureNames(alldata[[2]][[2]])
# 'H1_13C6_204_186' %in% featureNames(alldata[[1]][[1]])
# 'H1_13C6_204_186' %in% featureNames(alldata[[1]][[2]])
#
# 'H1' %in% featureNames(alldata[[2]][[1]])
# 'H1' %in% featureNames(alldata[[2]][[2]])
# 'H1' %in% featureNames(alldata[[1]][[1]])
# 'H1' %in% featureNames(alldata[[1]][[2]])
#
# sampleNames(alldata[[2]][[1]])
# sampleNames(alldata[[2]][[2]])
# sampleNames(alldata[[1]][[1]])


#filter(fName =='PC aa C24:0' & sampleType == '02' & fiaValue > 1e4) %>%
#c('PC aa C24:0','H1','C2')
#FIAspikes <- c('C0','C2','C3','C4','C5','C6 (C4:1-DC)','C8','C10','C12','C14','C16','C18','lysoPC a C18:0',
#'PC aa C24:0','SM C18:0','H1','PC aa C36:0')
#tst2 <-tst %>% filter(fName %in% c('C5') &
#                      sampleType == '01' )

# #View(tst)
# ggplot(tst2, aes( x =  barc_batch_date, y=fiaValue, color=sampleTypeName)) +
#   geom_point(alpha=0.5) +
#   theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5)) +
#   #scale_y_continuous(trans='log10') +
#   facet_wrap(vars(fName), ncol=1)
#
#

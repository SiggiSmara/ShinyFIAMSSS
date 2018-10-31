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
  return(retVals)
}


#' @name parseFilename
#'
#' @title Split a file name by '.' or '_' or '-'
#'
#' @param oneName the filename that needs to be parsed
#'
#' @details
#' Takes a filename ad splits it by one of three characters ('.' or '_' or '-').
#' TODO: add the return indices to the call
#'
#' @return a vector with the strings split from the file name
#'
# barcode, well, polarity, extra1, extra2, sample type, sample name
parseFilename <- function(oneName) {
  retVals <- unlist(str_split(oneName,"[._-]"))[c(4,5,6,7,8,9,15)]
  names(retVals) <- c("barcode","well","runNo" ,"extra1", "extra2", "sampleType", "sampleName")
  return(retVals)
}

#' @name parseDups
#'
#' @title parse out diplicates from a tibble
#'
#' @param fileTibble a tibble with columns of "barcode",
#' "well", "runNo", "extra1", "extra2", "sampleType", "sampleName"
#' coming from the parseFile function
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
parseDups <- function(fileTibble) {
  testCols <- c("barcode", "well", "runNo", "extra1", "extra2", "sampleType", "sampleName")
  testdata <- fileTibble %>% mutate(batchNo = -1) %>%
        arrange(barcode, well, runNo, extra1, extra2, sampleType, sampleName, tStamp)

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

#' @name readFiadata
#'
#' @title Read a collection of fia files, and split them into pos and neg polarities
#'
#' @param fiaFolderPath a path to a batch of mzML files to be read and process
#'
#' @details
#'
#' @return a list of possibly one or two xcms objects, one for each polarity containing the
#' respective mzML file information. The list items are named, either 'pos' and/or 'neg'
#' reflecting the polarity of the data. This is obviously assuming that one data file only
#' stores data from one polarity.
#'
readFiadata <- function(fiaFolderPath) {
  fls <- list.files(fiaFolderPath,'*.mzML')

  parsedFls <- as_tibble(t(apply(matrix(fls), MARGIN=1, FUN = parseFilename)))
  parsedFls <- parsedFls %>% mutate(tStamp = unlist(lapply(file.path(fiaFolderPath,fls),getStartTimeStamp)))
  parsedFls <- parseDups(parsedFls)
  if(length(unique(parsedFls$runNo)) == 1) {
    parsedFls <- parsedFls %>% mutate(runNo = 1)
  }
  parsedFls <- parsedFls %>% rename(polarity = runNo)
  posneg <- c(0,1)

  bothdata <- vector("list", length(posneg))
  names(bothdata) <- c('neg', 'pos')
  for(onePol in posneg) {
    polFls <- fls[parsedFls$polarity == onePol]
    if(length(polFls) > 0) {
      bothdata[[onePol+1]] <- readSRMData(
        file.path(fiaFolderPath, polFls))

      #assign sample names
      sampleNames(bothdata[[(onePol+1)]]) <- unlist(unite(parsedFls %>% filter(polarity == onePol), sep ="_"))

      #assign mzML features
      mzRfeatures <- as.tibble(as(featureData(bothdata[[(onePol+1)]]), "data.frame"))
      mzRfeatures <- mzRfeatures %>% mutate(Q1 = precursorIsolationWindowTargetMZ,
                                            Q3 = productIsolationWindowTargetMZ)
      biocrIdx <- rep(0, length(mzRfeatures$Q1))
      fnames <- rep("feature", length(mzRfeatures$Q1))
      for(h in 1:length(mzRfeatures$Q1) ) {
        mzRQ1 <- mzRfeatures$Q1[h]
        diffs <- abs(myBiocFeatures$Q1-mzRQ1)
        myfound <- which(diffs == min(diffs) & diffs < 0.25)
        foundOne <- FALSE
        for(j in myfound) {
          if(mzRfeatures$polarity[h] == myBiocFeatures$polarity[j] &
             abs(mzRfeatures$Q3[h] - myBiocFeatures$Q3[j]) < 0.25) {
            biocrIdx[h] <- j
            foundOne <- TRUE
            fnames[h] <- myBiocFeatures$name[j]
          }
        }
        if(!foundOne) {
          fnames[h] <- paste0("unknown_", mzRfeatures$Q1[h], "_", mzRfeatures$Q3[h])
          print(c("unknown transition:", mzRfeatures$Q1[h],mzRfeatures$Q3[h]))
        }
      }
      #then finally asign the feature names
      featureNames(bothdata[[(onePol+1)]]) <- fnames
    }
  }

  return(bothdata)
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
#' @return a tibble of results for all experiments found in the folder
#'
# test for a result table, if not found then
# generate one, otherwise just load the result table
# forceRecalc = TRUE skips the test and recalculates the
# result.
readOneFolder <- function(onePath, resultName = 'result.tsv', forceRecalc = FALSE) {
  resFilePath <- file.path(onePath, resultName)
  oneResdata <- NULL
  #print(c(!file.exists(resFilePath) , forceRecalc == TRUE))
  if(!file.exists(resFilePath) | forceRecalc == TRUE) {
    unlink(resFilePath)
    oneData <- readFiadata(onePath)
    for(onePol in oneData) {
      if(length(onePol)>0) {
        oneResdata  <- bind_rows(oneResdata,calculateMeanValues(onePol))
      }
    }
    write.table(oneResdata, resFilePath, row.names = FALSE, sep = '\t')
  } else {
    oneResdata <- read_tsv(resFilePath)
  }
  return(oneResdata)
}

#' @name findPotentialWiffDirs
#'
#' @title Find wiff directories that may contain SS batches
#'
#' @param parentPath a file path that contains wiff data
#' @param doConvert boolean. If TRUE then msconvert will be called on
#' batches that are not present in the working directory.
#' @param forceRecalc boolean. If TRUE then all batches will be
#' reconverted with msconvert. This means that any results are also
#' lost.
#'
#' @details
#'
#' DONE: find all folders that have at least the spiked sample in them
#' DONE: find all wiff folders that have equal numbers of spikes and blanks
#' DONE: check if a barcode folder is present in MZML_PATH corresponding to a wiff folder
#'
#' @return a tibble that contains all folders that potentially have full sets of SS
#' batches along with the information if that folder is found in the working directory
findPotentialWiffDirs <- function(parentPath, doConvert = FALSE, forceRecalc = FALSE) {
  if(!dir.exists(parentPath)) return()
  ss_spike_name <- "20000004\\.wiff"
  blank_name <-  "10000001\\.wiff"

  ss_namelen <- nchar(ss_spike_name)
  blank_nameLen <- nchar(blank_name)
  prec_len <- nchar(parentPath)+1

  foundFiles = list.files(parentPath, pattern = ss_spike_name, recursive = TRUE)
  for(i in 1:length(foundFiles)) {
    foundFiles[i] <- dirname(foundFiles[i])
  }
  foundFiles <- unique(foundFiles)
  goodFolders <- rep('',length(foundFiles))

  res <- tibble(folder = character(), barcode = character(), converted = logical(), fullPath=character())
  for(i in 1:length(goodFolders)) {
    spikeFounds <- list.files(file.path(parentPath,foundFiles[i]), pattern = ss_spike_name)
    blankFounds <- list.files(file.path(parentPath,foundFiles[i]), pattern = blank_name)
    if(length(spikeFounds) == length(blankFounds)) {
      folder = basename(foundFiles[i])
      if(str_detect(folder,'FIA')) {
        #print(folder)
        folder <- basename(dirname(file.path(parentPath,foundFiles[i])))
        #print(folder)
      }
      barcodeVector <- unique(unlist(lapply(str_split(spikeFounds,'_'), FUN = function(x) {x[2]})))
      for(barcode in barcodeVector) {
        converted <- dir.exists(file.path(MZML_PATH, folder, barcode))
        res <- bind_rows(res, tibble( folder = folder,
                                      barcode = barcode,
                                      converted = converted,
                                      fullPath = file.path(parentPath,foundFiles[i])
        )
        )
      }
    }
  }
  if(doConvert) {
    unconverts <- res %>% filter(converted == FALSE)
    if(forceRecalc) {
      unconverts <- res
    }
    if(dim(unconverts)[1]>0){
      apply(unconverts, MARGIN=1, FUN=convertOneWiffFolder)
    }
  }
  return(res)
}



#' @name convertOneWiffFolder
#'
#' @title Call proteowizard msconvert to extract the data
#'
#' @param myTibbleRow a row from the findPotentialWiffDirs tibble that is returned
#'
#' @details
#' Calling the msconvert will create a folder in the working directory named by the date
#' and the barcode of the batch.
#'
#' @return Nothing is returned
#parPath <- '/media/ssmarason/qtrap/Analyst Data/Projects/CHRIS_Biocrates'
#allWiffPaths <- findPotentialWiffDirs(parPath, doConvert=TRUE)
#allWiffPaths %>% filter(converted == FALSE)
# DONE: call the proteowizard msconvert to convert wiff files to mzML files and put them in the right barcode folder
convertOneWiffFolder <- function(myTibbleRow) {

  wiffPath <- myTibbleRow['fullPath']
  destPath <- file.path(MZML_PATH, myTibbleRow['folder'], myTibbleRow['barcode'])
  unlink(destPath, recursive = TRUE)
  dir.create(destPath, recursive = TRUE)
  if(.Platform$OS.type == 'windows'){
    command <- "c:/Program Files/ProteoWizard/ProteoWizard 3.0.18271.75bc4c4ea/msconvert"
    args <- paste(sprintf('"%s"',file.path(wiffPath,paste0('KIT*_',myTibbleRow['barcode'],'*.wiff'))),
                  '-z',
                  '-o',
                  sprintf('"%s"',destPath)
    )
  } else {
    command <- 'wine'
    args <- paste('"c:/Program Files/ProteoWizard/ProteoWizard 3.0.18271.75bc4c4ea/msconvert"',
                  sprintf('"%s"',file.path(wiffPath,paste0('KIT*_',myTibbleRow['barcode'],'*.wiff'))),
                  '-z',
                  '-o',
                  sprintf('"%s"',destPath)
    )
  }
  suppressWarnings({
    system2(command, args)
  })
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
reloadFiaResults <- function(forceRecalc = FALSE) {
  #read the mzML files and the cps data from biocrates
  datafolders <- list.dirs(MZML_PATH)
  datafolders <- datafolders[grep("[0-9]{5}",basename(datafolders))]
  resdata <- NULL
  for(fpath in datafolders) {
    print(fpath)
    resdata  <- bind_rows(resdata,readOneFolder(fpath, forceRecalc = forceRecalc))
  }

  resdata <- resdata %>% separate(sName,
                                  c("barcode",
                                    "well",
                                    "polarity",
                                    "extra1",
                                    "extra2",
                                    "sampleType",
                                    "sampleName",
                                    "tStamp",
                                    "batchNo"),
                                  sep="_",
                                  remove=TRUE)

  resdata <- mutate(resdata, included = 1,
                    realDate = ymd_hms(tStamp),
                    sampleTypeName = ifelse(sampleType =='01','Blank','SS')
  )

  resdata$included[resdata$batchNo>1] = 0
  resdata$included[is.na(resdata$fiaValue)] = 0
  resdata$included[resdata$barcode =='1015693595'] = 0
  resdata$included[resdata$barcode =='1026741254'] = 0
  resdata$included[resdata$barcode =='1026743063'] = 0
  resdata$included[resdata$barcode =='1015705381' & resdata$well == '06'] = 0
  resdata$included[resdata$barcode =='1015709562' & resdata$well == '68'] = 0
  resdata$included[resdata$barcode =='1023158371' & resdata$well == '02'] = 0
  resdata$included[resdata$barcode =='1026743112' & resdata$well == '14'] = 0
  resdata$included[resdata$barcode =='1026743112' & resdata$well == '02'] = 0

  tst <- resdata %>%
    filter(included == 1 ) %>%
    group_by(fName, sampleTypeName, polarity) %>%
    mutate(grpMedVal = median(fiaValue),
           fiaValueRLA = fiaValue/grpMedVal
           ) %>%
    ungroup()

  tst <- unite(tst, 'type_pol', c('sampleTypeName', 'polarity'), remove = FALSE)

  tst <- tst %>%
    group_by(fName, barcode, batchNo) %>%
    mutate(batchDate = min(realDate),
           barc_batch_date = paste(barcode,
                                   batchNo,
                                   year(batchDate),
                                   month(batchDate),
                                   day(batchDate),
                                   sep='_')) %>%
    ungroup()

  tst <- tst %>%
    group_by(fName, sampleTypeName, type_pol, barc_batch_date) %>%
    arrange(barcode, batchNo, realDate)

  save(resdata, file=file.path(MZML_PATH, 'resdata.RData'))
  save(tst, file=file.path(MZML_PATH, 'tst.RData'))

}


#' @name loadFiaResults
#'
#' @title Load the FIA results
#'
#' @param forceRecalc boolean. Passed to \code{\link{reloadFiaResults}}.
#'
#' @details
#' Load the FIA resutls, passes the forceRecalc variable to the underlying
#' reloadFiaResults function that is called if the RData files are not found
#'
#' @return no return value, assigns to global values globalResdata
#' and globalResdataNice
#'
loadFiaResults <- function(forceRecalc = FALSE) {
  loaded <- FALSE
  if(file.exists(file.path(MZML_PATH, 'resdata.RData'))) {
    load(file.path(MZML_PATH, 'resdata.RData'))
    loaded <- TRUE
  }
  if(file.exists(file.path(MZML_PATH, 'tst.RData'))) {
    load(file.path(MZML_PATH, 'tst.RData'))
    loaded <- TRUE & loaded
  }

  if(!loaded) {
    reloadFiaResults(forceRecalc = forceRecalc)
    load(file.path(MZML_PATH, 'resdata.RData'))
    load(file.path(MZML_PATH, 'tst.RData'))
  }
  assign('globalResdata', resdata, inherits = TRUE)
  assign('globalResdataNice', tst, inherits = TRUE)
}

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

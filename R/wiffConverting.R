#' @name findPotentialWiffDirs
#'
#' @title Find wiff directories that may contain SS batches
#'
#' @param parentPath a file path that contains wiff data
#' @param resPath a file path that contains the resulting mzML files
#' @param protwizPath a file path to the proteowizard installation
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
#' DONE: check if a barcode folder is present in resPath corresponding to a wiff folder
#'
#' @return a tibble that contains all folders that potentially have full sets of SS
#' batches along with the information if that folder is found in the working directory
findPotentialWiffDirs <- function(parentPath, resPath, protwizPath, doConvert = FALSE, forceRecalc = FALSE) {
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
        converted <- dir.exists(file.path(resPath, folder, barcode))
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
      apply(unconverts, MARGIN=1, FUN=convertOneWiffFolder, resPath = resPath, protwizPath = protwizPath)
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
convertOneWiffFolder <- function(myTibbleRow, resPath, protwizPath) {

  wiffPath <- myTibbleRow['fullPath']
  destPath <- file.path(resPath, myTibbleRow['folder'], myTibbleRow['barcode'])
  unlink(destPath, recursive = TRUE)
  dir.create(destPath, recursive = TRUE)
  if(.Platform$OS.type == 'windows'){
    command <- sprintf('"%s"',normalizePath(paste0(protwizPath,"/msconvert")))
    args <- paste(sprintf('"%s"',file.path(wiffPath,paste0('KIT*_',myTibbleRow['barcode'],'*.wiff'))),
                  '-z',
                  '-o',
                  sprintf('"%s"',destPath)
    )
  } else {
    command <- 'wine'
    args <- paste(sprintf('"%s"',normalizePath(paste0(protwizPath,"/msconvert"))),
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

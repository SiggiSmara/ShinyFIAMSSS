library(shiny)
library(R6)

source('test/app.R')
source('app2.R')
test <- R6Class(
  "test",
  public = list(
    MZML_PATH = NULL,
    startShiny = function(aPath) {
      self$MZML_PATH <- aPath
      #runApp(getApp(self))
      runApp(getApp2(self))
    })
)




tt <- test$new()

tt$startShiny('test test')

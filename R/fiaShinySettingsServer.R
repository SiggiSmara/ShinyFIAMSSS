#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
# Define server logic required to draw a histogram
fiaShinySettingsServer <- function(self) {
  return(function(input, output, session) {

    output$note <- renderText({ "* Note: should be present in the Result directory" })

    startupFilter <- reactive({
      genText <- paste("This Shiny App is intended to be run",
                       "as a part of a larger workflow in which some objects would",
                       "already be defined in this environment prior to `runApp` being",
                       "executed. Try evaluating the code in `parent.R` which",
                       "wraps this Shiny app in a larger workflow.")
      if (!"workdirPath" %in% names(self$settings)) {
        stop(paste("'workdirPath' var doesn't exist.",genText))
      }
      #populate the settings
      updateDirectoryInput(session, 'mzmlDirectory', value = self$settings$workdirPath)

      if(file.exists(file.path(self$settings$workdirRDataPath,'settings.RData'))) {
        load(file.path(self$settings$workdirRDataPath,'settings.RData'))
        self$settings <- settings
      } else {
        showNotification("No settings file detected. Using defaults.", type='warning')
      }
      settings <- self$settings
      updateDirectoryInput(session, 'wiffDirectory', value = as.character(self$settings$wiffPath))
      updateDirectoryInput(session, 'protwizDirectory', value = as.character(self$settings$protwizPath))
      updateCheckboxInput(session,'convertNewWiffs', value =as.logical(self$settings$convertWiffs))
      #updateCheckboxInput(session,'useParallel', value =as.logical(self$settings$useParallel))
      updateTextInput(session, 'fiaFile', value = as.character(self$settings$fiaFile))
      #updateTextInput(session, 'multicores', value = as.character(self$settings$multicores))
      #updateTextInput(session, 'fiaIstdFile', value = as.character(self$settings$fiaIstdFile))
      updateTextAreaInput(session, 'fiaFeatures', value = as.character(self$settings$fiaFeatures))



      # loadFiaResults()
      #
      # myUIdata <- get('globalUIdata')
      # myUIdata$allDates <- globalResdataNice %>% group_by(batchDate) %>% summarise()
      # myUIdata$allDates <-myUIdata$allDates$batchDate
      # myUIdata$allYears <- unique(year(myUIdata$allDates))
      # assign('globalUIdata', myUIdata, inherits=TRUE)
      #

      return('')
    })

    output$filter2 <- renderText({
      startupFilter()
    })

    observe({
      if (input$Continue > 0) {
        self$settings$fiaFile <- input$fiaFile
        #self$settings$fiaIstdFile <- input$fiaIstdFile
        self$settings$fiaFeatures <- unlist(str_split(input$fiaFeatures,','))
        self$settings$workdirPath <- readDirectoryInput(session, 'mzmlDirectory')
        self$settings$wiffPath <- readDirectoryInput(session, 'wiffDirectory')
        self$settings$convertWiffs <- input$convertNewWiffs
        #self$settings$useParallel <- input$useParallel
        #self$settings$multicores <- input$multicores
        stopApp()  # stop shiny
      }
    })

    observeEvent(
      ignoreNULL = TRUE,
      eventExpr = {
        input$mzmlDirectory
      },
      handlerExpr = {
        # condition prevents handler execution on initial app launch
        if (input$mzmlDirectory > 0) {
          # launch the directory selection dialog with initial path read from the widget
          path = choose.dir(default = readDirectoryInput(session, 'mzmlDirectory'),
                            'Select the result directory...')

          # update the widget value
          updateDirectoryInput(session, 'mzmlDirectory', value = path)
        }
      }
    )

    observeEvent(
      ignoreNULL = TRUE,
      eventExpr = {
        input$wiffDirectory
      },
      handlerExpr = {
        # condition prevents handler execution on initial app launch
        if (input$wiffDirectory > 0) {
          # launch the directory selection dialog with initial path read from the widget
          path = choose.dir(default = readDirectoryInput(session, 'wiffDirectory'),
                            'Select the raw data directory...')

          # update the widget value
          updateDirectoryInput(session, 'wiffDirectory', value = path)
        }
      }
    )

    observeEvent(
      ignoreNULL = TRUE,
      eventExpr = {
        input$protwizDirectory
      },
      handlerExpr = {
        # condition prevents handler execution on initial app launch
        if (input$protwizDirectory > 0) {
          # launch the directory selection dialog with initial path read from the widget
          path = choose.dir(default = readDirectoryInput(session, 'protwizDirectory'),
                            'Select the Proteowizard directory...')

          # update the widget value
          updateDirectoryInput(session, 'protwizDirectory', value = path)
        }
      }
    )

    observeEvent(input$fiaFile, {
      if(!file.exists(file.path(self$settings$workdirPath,input$fiaFile))) {
        shinyjs::runjs(paste0("document.getElementById('fiaFile').style.border =
                     'solid red'"))
      } else {
        shinyjs::runjs(paste0("document.getElementById('fiaFile').style.border =
                     ''"))
      }
      })

    # observeEvent(input$fiaIstdFile, {
    #   if(!file.exists(file.path(self$settings$workdirPath,input$fiaIstdFile))) {
    #     shinyjs::runjs(paste0("document.getElementById('fiaIstdFile').style.border =
    #                  'solid red'"))
    #   } else {
    #     shinyjs::runjs(paste0("document.getElementById('fiaIstdFile').style.border =
    #                  ''"))
    #   }
    #   })



    observeEvent(input$recalcAll, {
      # session$sendCustomMessage(type = 'testmessage',
      #                           message = 'Recalculating... this takes time')
      reloadResults(TRUE)
    })

    observeEvent(input$saveSettings, {
      settings <- self$settings
      self$settings$fiaFile <- input$fiaFile
      #self$settings$fiaIstdFile <- input$fiaIstdFile
      self$settings$fiaFeatures <- unlist(str_split(input$fiaFeatures,','))
      self$settings$workdirPath <- readDirectoryInput(session, 'mzmlDirectory')
      self$settings$wiffPath <- readDirectoryInput(session, 'wiffDirectory')
      self$settings$convertWiffs <- input$convertNewWiffs
      #self$settings$useParallel <- input$useParallel
      #self$settings$multicores <- input$multicores

      save(settings, file=file.path(self$settings$workdirRDataPath,'settings.RData'))
      showNotification("Settings saved", type='message')
    })
    })
}


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
fiaShinyServer <- function(self) {
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
    updateTextInput(session, 'reloadData', value = as.logical(self$settings$reloadData))
    updateTextInput(session, 'forceRecalc', value = as.logical(self$settings$forceRecalc))
    updateTextAreaInput(session, 'fiaFeatures', value = as.character(self$settings$fiaFeatures))
    return('')
  })

  startupFilter2 <- reactive({
    updateSelectInput(session, 'metaboliteID', choices = myAnalytes())
    #updateSelectInput(session, 'filterYear', choices = c('select a year',allYears()))
    updateSelectInput(session, 'batchName', choices = self$myUIdata$allBatchNames)
    updateSelectInput(session, 'batchID', choices = myBatches())
    return('')
  })

  output$filter <- renderText({
    startupFilter()
  })

  output$filter2 <- renderText({
    startupFilter2()
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

  observeEvent(input$saveSettings, {
    settings <- self$settings
    self$settings$fiaFile <- input$fiaFile
    #self$settings$fiaIstdFile <- input$fiaIstdFile
    self$settings$fiaFeatures <- unlist(str_split(input$fiaFeatures,','))
    self$settings$workdirPath <- readDirectoryInput(session, 'mzmlDirectory')
    self$settings$wiffPath <- readDirectoryInput(session, 'wiffDirectory')
    self$settings$convertWiffs <- input$convertNewWiffs
    self$settings$reloadData <- input$reloadData
    self$settings$forceRecalc <- input$forceRecalc

    save(settings, file=file.path(self$settings$workdirRDataPath,'settings.RData'))
    showNotification("Settings saved", type='message')
  })

  observe({
    if (input$Continue > 0) {
      self$settings$fiaFile <- input$fiaFile
      #self$settings$fiaIstdFile <- input$fiaIstdFile
      self$settings$fiaFeatures <- unlist(str_split(input$fiaFeatures,','))
      self$settings$workdirPath <- readDirectoryInput(session, 'mzmlDirectory')
      self$settings$wiffPath <- readDirectoryInput(session, 'wiffDirectory')
      self$settings$convertWiffs <- input$convertNewWiffs
      self$settings$reloadData <- input$reloadData
      self$settings$forceRecalc <- input$forceRecalc

      # Create a Progress object
      progress <- shiny::Progress$new()
      progress$set(message = "Preparing data", value = 0)
      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())

      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
      }
      self$prepareForFIA(updateProgress)

      updateTabsetPanel(session, "inTabset",
                        selected = "Time Trends")
      #switch to next panel?
      #stopApp()  # stop shiny
    }
  })



  observeEvent(input$batchName, {
    updateSelectInput(session, 'batchID', choices = myBatches())
  })


  myBatches <- reactive({
    allBatches <- self$myUIdata$allBatches
    if(input$batchName !='##########') {
      allBatches <- allBatches %>% filter(as.character(batchName) == input$batchName)
    }
    return(unique(allBatches$barcode))
  })

  myBatchData <- reactive({
    req(input$sampleTypes)
    req(input$batchID)
    req(input$batchName)
    if(input$batchID !='##########') {
      firstPass <- filter(self$resdataNice, barcode == input$batchID) %>%
        filter(batchName == input$batchName) %>%
        filter(sampleTypeName %in% as.factor(input$sampleTypes )) %>%
        filter(fName %in% myAnalytes())
      if(input$valueType =='Absolute') {
        firstPass <- mutate(firstPass, displayValue = fiaValue)
      } else {
        firstPass <- mutate(firstPass, displayValue = fiaValueRLA)
      }
      return(firstPass)
    } else {
      return(firstPass <- self$resdataNice %>% filter(fName %in% ''))
    }
  })

  observeEvent(input$metaboTypes, {
    newAnalytes = myAnalytes()
    if(any(input$metaboliteID %in% newAnalytes)) {
      updateSelectInput(session, 'metaboliteID', choices = newAnalytes, selected = input$metaboliteID)
    } else {
      updateSelectInput(session, 'metaboliteID', choices = newAnalytes, selected = newAnalytes[1])
    }

  }, ignoreNULL = FALSE)

  allYears <- reactive({
    return(self$myUIdata$allYears)
  })

  myAnalytes <- reactive({
    analytes <- NULL
    if('Analytes' %in% input$metaboTypes) {
      analytes <- c(analytes, self$settings$fiaFeatures)
    }
    if('ISTDs' %in% input$metaboTypes) {
      analytes <- c(analytes, self$myUIdata$ISTDs)
    }
    return(analytes)
  })



  tst2 <- reactive({
    if(length(input$metaboliteID) == 0) {
      firstPass <- self$resdataNice %>% filter(fName %in% '')
    } else {
      req(input$metaboliteID)
      req(input$sampleTypes)
      #req(input$valueType)
      firstPass <- self$resdataNice %>%
                        filter(as.character(fName) %in% input$metaboliteID &
                               sampleTypeName %in% as.factor(input$sampleTypes ))

    }

    return(firstPass)
  })

  barcodeOverview <- reactive({
    firstPass <- tst2() %>%
      mutate(sampleType = sampleTypeName) %>%
      group_by(batchName,
               batchDate,
               barcode,
               sampleType) %>%
      summarise(medAbsValue = round(median(fiaValue)),
                medRelValue = round(median(fiaValueRLA),2),
                included = mean(included)
      )
    return(firstPass)
  })

  ranges <- reactiveValues(x = NULL, y = NULL, inclChanged = FALSE)

  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$timePlot_dblclick, {
    brush <- input$timePlot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })

  output$indivPlot <- renderPlot({
    mydata <- myBatchData()
    if(input$valueType =='Absolute') {
      mydata <- mutate(mydata, displayValue = fiaValue)
    } else {
      mydata <- mutate(mydata, displayValue = fiaValueRLA)
    }
    if(length(unique(mydata$type_pol))>0){
      ggplot(mydata, aes( x = tStamp, y=displayValue, color=fName, group=fName)) +
        geom_point(alpha=0.5) +
        geom_line()+
        ggtitle(paste0("SS batch: ",
                       paste(unique(mydata$barcode), sep=','),
                        " metabolite: ",
                       paste(unique(mydata$fName), sep=',' ))) +
        theme(plot.title = element_text(hjust = 0.5)) +
        facet_wrap(. ~ type_pol, nrow=2)
    }
  })

  output$timePlot <- renderPlot({
    mydata <- tst2() %>% filter(included == 1)
    if(input$valueType =='Absolute') {
      mydata <- mutate(mydata, displayValue = fiaValue)
    } else {
      mydata <- mutate(mydata, displayValue = fiaValueRLA)
    }
    ggplot(mydata, aes( x =  barc_batch_bname, y=displayValue, color=type_pol)) +
         geom_boxplot(alpha=0.5) +
         theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5)) +
         #scale_y_continuous(trans='log10') +
        ggtitle(paste0("SS overview of:",
                       paste(unique(mydata$fName),sep=', '))) +
        theme(plot.title = element_text(hjust = 0.5)) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)

  })

  output$table <- DT::renderDT({
    DT::datatable(barcodeOverview())
    if(ranges$inclChanged) {
      isolate(ranges$inclChanged <- FALSE)
    }
  })

  observeEvent(input$toggleState, {
    mydata <- barcodeOverview()[input$table_rows_selected,]
    myBarcodes <- unique(unlist(mydata$barcode, use.names = FALSE))
    myIndices <- which(self$resdataNice$barcode %in% myBarcodes)
    origIncludes <- self$resdataNice$included[myIndices]
    self$resdataNice$included[myIndices] <- abs(origIncludes-1)
    self$resdataNice <- self$resdataNice %>%
      filter(included == 1 ) %>%
      group_by(fName, sampleTypeName, polarity) %>%
      mutate(grpMedVal = median(fiaValue),
             fiaValueRLA = fiaValue/grpMedVal
      ) %>%
      ungroup()
    ranges$inclChanged <- TRUE
  })

})

}

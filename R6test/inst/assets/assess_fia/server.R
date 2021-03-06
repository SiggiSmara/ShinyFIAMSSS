#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  startupFilter <- reactive({
    updateSelectInput(session, 'metaboliteID', choices = myAnalytes())
    updateSelectInput(session, 'filterYear', choices = c('select a year',allYears()))
    updateSelectInput(session, 'batchID', choices = myBatches(), selected = input$batchID)
    return('')
  })
  
  output$filter2 <- renderText({ 
    startupFilter()
  })
  
  observeEvent(input$filterYear, {
    if(length(globalUIdata$allDates) > 0){
      mymonths <- globalUIdata$allDates[year(globalUIdata$allDates) == input$filterYear]
      mymonths <-unique(month(mymonths, label = TRUE, abbr = TRUE))
      updateSelectInput(session, 'filterMonth', choices = c('select a month', as.character(mymonths)))
      updateSelectInput(session, 'batchID', choices = myBatches(), selected = input$batchID)
    }
  })
  
  observeEvent(input$filterMonth, {
    if(length(globalUIdata$allDates) > 0) {
      mymonths <- globalUIdata$allDates[year(globalUIdata$allDates) == input$filterYear]
      mydays <- mymonths[month(mymonths, label = TRUE, abbr = TRUE) == input$filterMonth] 
      mydays <-unique(day(mydays))
      updateSelectInput(session, 'filterDay', choices = c('select a day',mydays))
      updateSelectInput(session, 'batchID', choices = myBatches(), selected = input$batchID)
    }
  })
  
  observeEvent(input$filterDay, {
    if(length(globalUIdata$allDates) > 0) {
      updateSelectInput(session, 'batchID', choices = myBatches(), selected = input$batchID)
    }
  })
  
  myBatches <- reactive({
    allBatches <- globalResdataNice %>% group_by(barcode, batchDate) %>% summarise()
    if(input$filterYear !='select a year') {
      #print("filter year")
      allBatches <- allBatches %>% filter(year(batchDate) == input$filterYear)
      if(input$filterMonth != 'select a month') {
        #print("filter month")
        allBatches <- allBatches %>% filter(month(batchDate, label = TRUE, abbr = TRUE) == input$filterMonth)
        if(input$filterDay != 'select a day') {
          #print("filter day")
          allBatches <- allBatches %>% filter(day(batchDate) == input$filterDay)
        }
      }
    }
    return(unique(allBatches$barcode))
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
    return(globalUIdata$allYears)
  })
  
  myAnalytes <- reactive({
    analytes <- NULL
    if('Analytes' %in% input$metaboTypes) {
      analytes <- c(analytes, globalSettings$fiaFeatures)
    }
    if('ISTDs' %in% input$metaboTypes) {
      analytes <- c(analytes, FIAistds$SHORT_NAME)
    }
    return(analytes)
  })
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$mzmlDirectory
    },
    handlerExpr = {
      if (input$mzmlDirectory > 0) {
        # condition prevents handler execution on initial app launch
        
        # launch the directory selection dialog with initial path read from the widget
        path = choose.dir(default = readDirectoryInput(session, 'mzmlDirectory'),
                          'Select the result directory...')
        
        # update the widget value
        updateDirectoryInput(session, 'mzmlDirectory', value = path)
      }
    }
  )
  
  observeEvent(input$recalcAll, {
    # session$sendCustomMessage(type = 'testmessage',
    #                           message = 'Recalculating... this takes time')
    reloadResults(TRUE)
  })
  
  observeEvent(input$saveSettings, {
    settingsFilePath <- file.path(MZML_PATH,'settings.RData')
    settings <- c()
    settings$fiaFile = input$fiaFile
    settings$fiaIstdFile = input$fiaIstdFile
    settings$fiaFeatures = input$fiaFeatures
    save(settings, file=settingsFilePath)
    showNotification("Settings saved", type='message')
  })
  
  tst2 <- reactive({
    if(length(input$metaboliteID) == 0) {
      return(globalResdataNice %>% filter(fName %in% '') )
    } else {
      req(input$metaboliteID)
      req(input$sampleTypes)
      return(globalResdataNice %>% filter(fName %in% input$metaboliteID &
                            sampleTypeName %in% input$sampleTypes )
      )
    }
  })
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
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
  
  output$timePlot <- renderPlot({
    mydata <- tst2()
    ggplot(mydata, aes( x =  barc_batch_date, y=fiaValue, color=type_pol)) +
         geom_boxplot(alpha=0.5) +
         theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5)) +
         #scale_y_continuous(trans='log10') +
        ggtitle(paste0("SS overview of:",
                       paste(unique(mydata$fName),sep=', '))) +
        theme(plot.title = element_text(hjust = 0.5)) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
    
  })
  
  output$table <- DT::renderDT({
    DT::datatable(tst2() %>% select(batchDate, 
                                    barcode, 
                                    sampleTypeName, 
                                    well, 
                                    fName, 
                                    polarity,
                                    fiaValue, 
                                    included )
                  )
  })
})

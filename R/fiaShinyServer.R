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

  startupFilter <- reactive({
    updateSelectInput(session, 'metaboliteID', choices = myAnalytes())
    #updateSelectInput(session, 'filterYear', choices = c('select a year',allYears()))
    updateSelectInput(session, 'batchName', choices = self$myUIdata$allBatchNames)
    updateSelectInput(session, 'batchID', choices = myBatches())
    return('')
  })

  output$filter2 <- renderText({
    startupFilter()
  })

  # observeEvent(input$filterYear, {
  #   if(length(self$myUIdata$allDates) > 0){
  #     mymonths <- self$myUIdata$allDates[year(self$myUIdata$allDates) == input$filterYear]
  #     mymonths <-unique(month(mymonths, label = TRUE, abbr = TRUE))
  #     updateSelectInput(session, 'filterMonth', choices = c('select a month', as.character(mymonths)))
  #     updateSelectInput(session, 'batchID', choices = myBatches(), selected = input$batchID)
  #   }
  # })

  # observeEvent(input$filterMonth, {
  #   if(length(self$myUIdata$allDates) > 0) {
  #     mymonths <- self$myUIdata$allDates[year(self$myUIdata$allDates) == input$filterYear]
  #     mydays <- mymonths[month(mymonths, label = TRUE, abbr = TRUE) == input$filterMonth]
  #     mydays <-unique(day(mydays))
  #     updateSelectInput(session, 'filterDay', choices = c('select a day',mydays))
  #     updateSelectInput(session, 'batchID', choices = myBatches(), selected = input$batchID)
  #   }
  # })

  # observeEvent(input$filterDay, {
  #   if(length(self$myUIdata$allDates) > 0) {
  #     updateSelectInput(session, 'batchID', choices = myBatches(), selected = input$batchID)
  #   }
  # })

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
      return(firstPass %>% filter(barcode == input$batchID))
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
    ggplot(mydata, aes( x = tStamp, y=displayValue, color=fName, group=fName)) +
      geom_point(alpha=0.5) +
      geom_line()+
      ggtitle(paste0("SS batch: ",
                     paste(unique(mydata$barcode), sep=','),
                      " metabolite: ",
                     paste(unique(mydata$fName), sep=',' ))) +
      theme(plot.title = element_text(hjust = 0.5)) +
      facet_wrap(. ~ type_pol, nrow=2)
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
    if(ranges$inclChanged) {
      isolate(ranges$inclChanged <- FALSE)
    }
    DT::datatable(barcodeOverview())
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
    #ranges$inclChanged <- TRUE
  })

})

}

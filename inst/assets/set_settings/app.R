#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
          #actionLink("recalcAll", "Recalc All"),
          directoryInput('mzmlDirectory', 
                         label = 'Results directory'
          ),
          textInput('fiaFile', 'MRM transitions file name **'),
          textInput('fiaIstdFile', 'ISTD intensity cutoff file name **'),
          tags$div(class="h5", checked=NA,
                   tags$p("** Note: should be present in the Results directory")
          ),
          textAreaInput('fiaFeatures', 'Features for SS check'),
          actionButton("saveSettings",'Save'),
          
        tags$button(
          id = 'close',
          type = "button",
          class = "btn action-button",
          onclick = "setTimeout(function(){window.close();},500);",  # close browser
          "Close window"
        ),
        width = 10
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        textOutput("filter2")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$note <- renderText({ "* Note: should be present in the Result directory" })
  
  startupFilter <- reactive({
    genText <- paste("This Shiny App is intended to be run",
                     "as a part of a larger workflow in which some objects would",
                     "already be defined in this environment prior to `runApp` being",
                     "executed. Try evaluating the code in `parent.R` which",
                     "wraps this Shiny app in a larger workflow.")
    if (!exists("MZML_PATH")) {
      stop(paste("'MZML_PATH' var doesn't exist.",genText))
    }
    #populate the settings
    updateDirectoryInput(session, 'mzmlDirectory', value = MZML_PATH)
    
    if(file.exists(file.path(MZML_PATH,'settings.RData'))) {
      load(file.path(MZML_PATH,'settings.RData'))
    } else {
      showNotification("No settings file detected. Using defaults.", type='warning')
    }
    updateTextInput(session, 'fiaFile', value = as.character(settings$fiaFile))
    updateTextInput(session, 'fiaIstdFile', value = as.character(settings$fiaIstdFile))
    updateTextAreaInput(session, 'fiaFeatures', value = as.character(settings$fiaFeatures))
    assign('globalSettings', settings, inherits = TRUE)
    
    
    
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
    if (input$close > 0) stopApp()  # stop shiny
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
        #path = choose.dir(default = readDirectoryInput(session, 'mzmlDirectory'),
        #                  'Select the result directory...')
        
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
}

# Run the application 
shinyApp(ui = ui, server = server)


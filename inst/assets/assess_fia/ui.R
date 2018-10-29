#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

navbarPage("FIA SS Explorer",
           tabPanel("Settings",
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
                        
                        width = 10
                      ),
                      
                      # Empty but used to call the filter check
                      # that also updates the inputs
                      mainPanel(
                        textOutput("filter2")
                        )
                    )
                    
            ),
           tabPanel("Time Trends",
                    # Sidebar for the time trend explorer
                    sidebarLayout(
                      sidebarPanel(
                        #actionButton("recalcAll", "Recalc All"),
                        checkboxGroupInput(inputId='metaboTypes', 
                                           label = 'Metabolyte types',
                                           c('Analytes','ISTDs'),
                                           selected = 'Analytes'
                        ),
                        selectInput('metaboliteID', 'Choose your analyte', 
                                    c('Analyte')
                        ),
                        checkboxGroupInput(inputId='sampleTypes', 
                                           label = 'Sample types',
                                           c('SS','Blank'),
                                           selected = 'SS'
                        ),
                        width = 3
                      ),
                      
                      # Show a plot of the generated distribution
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot", plotOutput("timePlot",
                                                                dblclick = "timePlot_dblclick",
                                                                brush = brushOpts(
                                                                  id = "timePlot_brush",
                                                                  resetOnNew = TRUE
                                                                ))),
                                    tabPanel("Table", DT::dataTableOutput("table"))
                        )
                      )
                    )
           ),
           tabPanel("Individual SS",
                    sidebarLayout(
                      sidebarPanel(
                        #actionButton("recalcAll", "Recalc All"),
                        selectInput('filterYear', 'Filter on year', 
                                    c(2015)
                        ),
                        selectInput('filterMonth', 'Filter on month', 
                                    c(-1:-12)
                        ),
                        selectInput('filterDay', 'Filter on day', 
                                    c(-1:-30)
                        ),
                        selectInput('batchID', 'Choose a batch', 
                                    c('##########')
                        ),
                        checkboxGroupInput(inputId='sampleTypes', 
                                           label = 'Sample types',
                                           c('SS','Blank'),
                                           selected = 'SS'
                        )
                      ),
                      
                      # Show a plot of the generated distribution
                      mainPanel(
                        
                      )
                    )
           )
           #,
           # navbarMenu("More",
           #            tabPanel("Table",
           #                     "table"
           #            ),
           #            tabPanel("About",
           #                     '<h3>About</h3>'
           #            )
           # )
)

# # Define UI for application that draws a histogram
# shinyUI(fluidPage(
#   
#   # Application title
#   titlePanel("FIA SS time trend explorer"),
#   
#   
# ))

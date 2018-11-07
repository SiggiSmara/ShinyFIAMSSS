#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinyjs)
fiaShinyUI <-function(self) {
  shinyUI(
    navbarPage("FIA SS Explorer",
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
                                           selected = 1
                        ),
                        selectInput(inputId='valueType',
                                           label = 'Display absolute or relative values',
                                           c('Absolute','Relative'),
                                           selected = 'Absolute'
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
                                                                )),
                                              textOutput("filter2")
                                             ),
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
  )
}


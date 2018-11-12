#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#library(shiny)
#library(shinyjs)
fiaShinyUI <-function(self) {
  shinyUI(
    navbarPage("FIA SS Explorer", id="inTabset",
          tabPanel("Settings",
                   sidebarLayout(
                     sidebarPanel(
                       #actionLink("recalcAll", "Recalc All"),
                       shinyjs::useShinyjs(),
                       directoryInput('mzmlDirectory',
                                      label = 'Results directory'
                       ),
                       directoryInput('wiffDirectory',
                                      label = 'Raw Data directory'
                       ),
                       directoryInput('protwizDirectory',
                                      label = 'Proteowizard directory'
                       ),
                       checkboxInput('convertNewWiffs', 'Look for and convert new raw data files', value = TRUE),
                       checkboxInput('reloadData', 'Reload results (Will only calculate new results if found)', value = FALSE),
                       checkboxInput('forceRecalc', 'Recalculate ALL results (Warning! This takes a long time)', value = FALSE),
                       # conditionalPanel(
                       #   condition = "input.useParallel == true",
                       #   textInput('multicores','How many cores to use?',value='3')
                       # ),
                       textInput('fiaFile', 'MRM transitions file name **'),
                       tags$div(class="h5", checked=NA,
                                tags$p("** Note: should be present in the Results directory")
                       ),
                       textAreaInput('fiaFeatures', 'Features for SS check'),
                       actionButton("saveSettings",'Save'),

                       tags$button(
                         id = 'Continue',
                         type = "button",
                         class = "btn action-button",
                         onclick = "setTimeout(function(){window.close();},500);",  # close browser
                         "Continue"
                       ),
                       width = 10
                     ),

                     # Show a plot of the generated distribution
                     mainPanel(
                       textOutput("filter")
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
                        checkboxGroupInput(inputId='sampleTypes',
                                           label = 'Sample types',
                                           c('SS','Blank'),
                                           selected = 'SS'
                        ),
                        selectInput('metaboliteID', 'Choose your analyte',
                                    c('Analyte')
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
                                    tabPanel("Table",
                                             actionButton("toggleState", "Toggle State"),
                                             hr(),
                                             DT::dataTableOutput("table")
                                             ),
                                    tabPanel("Individual",
                                             fluidRow(
                                               column(4,
                                                      selectInput('batchName', 'Choose a parent folder',
                                                                  c('##########')
                                                      )
                                                ),
                                               column(5,
                                                      selectInput('batchID', 'Choose a batch',
                                                                  c('##########')
                                                      )
                                                )
                                             ),

                                             hr(),

                                             plotOutput("indivPlot",
                                                        dblclick = "indivPlott_dblclick",
                                                        brush = brushOpts(
                                                          id = "indivPlot_brush",
                                                          resetOnNew = TRUE
                                                        ))
                                    )
                        )
                      )
                    )
           )
           #,
           # tabPanel("Individual SS",
           #          sidebarLayout(
           #            sidebarPanel(
           #              checkboxGroupInput(inputId='metaboTypes2',
           #                                 label = 'Metabolyte types',
           #                                 c('Analytes','ISTDs'),
           #                                 selected = 'Analytes'
           #              ),
           #              checkboxGroupInput(inputId='sampleTypes2',
           #                                 label = 'Sample types',
           #                                 c('SS','Blank'),
           #                                 selected = 'SS'
           #              ),
           #              #actionButton("recalcAll", "Recalc All"),
           #              # selectInput('filterYear', 'Filter on year',
           #              #             c(2015)
           #              # ),
           #              # selectInput('filterMonth', 'Filter on month',
           #              #             c(-1:-12)
           #              # ),
           #              # selectInput('filterDay', 'Filter on day',
           #              #             c(-1:-30)
           #              # ),
           #              selectInput('batchName', 'Choose a parent folder',
           #                          c('##########')
           #              ),
           #              selectInput('batchID', 'Choose a batch',
           #                          c('##########')
           #              )
           #
           #            ),
           #
           #            # Show a plot of the generated distribution
           #            mainPanel(
           #
           #            )
           #          )
           # )
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


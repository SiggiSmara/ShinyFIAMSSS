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
# Define UI for application that draws a histogram
fiaShinySettingsUI <-function(self) {
  shinyUI(fluidPage(

    # Application title
    titlePanel("FIA-MS System Suitability Settings"),

    # Sidebar with a slider input for number of bins
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
        textOutput("filter2")
      )
    )
  )
  )
}


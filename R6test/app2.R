library(shiny)
getApp2 <- function(self){
  source("test2/ui.R", local = TRUE)
  source("test2/server.R", local = TRUE)
  return(shinyApp(ui = uiFunc(), server = serverFunc))
}
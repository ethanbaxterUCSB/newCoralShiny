library(shiny)
source("defPars.R")
source("runCoral.R")

server <- function(input, output) {
  #Establish pars list with default parameter vectors
  pars <- list(HX=defPars_HX, S=defPars_S)
  
  updateInputsAndRun <- eventReactive(input$run,{
    #Update inputs
    
    #Run Coral
    
  })
  
  output$parsTest <- renderDataTable(pars$HX)
}
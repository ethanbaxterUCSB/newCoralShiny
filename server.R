library(shiny)
source("defPars.R")
source("runCoral.R")

server <- function(input, output) {
  #Establish pars list with default parameter vectors
  pars_HX <- defPars_HX
  pars_S <- defPars_S
  
  
  
  updateInputsAndRun <- eventReactive(input$run,{
    #Create environment descriptor lists
    L <- c(input$L[1], input$L[2], input$Lf)
    N <- c(input$N[1], input$N[2], input$Nf)
    X <- c(input$X[1], input$X[2], input$Xf)
    
    #Update inputs
    
    #Create time vector
    time <- c(t=0:(input$modelLength/dt))
    
    #Create initial environment
    env <- environment(time=time, L=L, N=N, X=X)
    
    #Run model
    runCoral(modelLength=input$modelLength, dt=input$dt, env=env,
             pars_HX=pars_HX, pars_S=pars_S)
  })
}
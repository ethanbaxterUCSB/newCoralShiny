library(shiny)
source("defPars.R")
source("runCoral.R")

server <- function(input, output) {
  #Establish parameter lists
  pars_HX <- defPars_HX
  pars_S <- defPars_S
  
  
  
  updateInputsAndRun <- observeEvent(input$run, {
    #Create environment descriptor lists from inputs
    L <- c(as.numeric(input$L[1]), as.numeric(input$L[2]), as.numeric(input$Lf))
    N <- c(as.numeric(input$N[1]*10^-6), as.numeric(input$N[2]*10^-6), as.numeric(input$Nf))
    X <- c(as.numeric(input$X[1]*10^-7), as.numeric(input$X[2]*10^-7), as.numeric(input$Xf))
    
    #Update inputs
    
    #Create time vector from inputs
    time <- seq(0,input$modelLength,input$dt)
    
    #Create initial environment
    env <- environment(time=time, L=L, N=N, X=X)
    
    #Run model
    coral <- runCoral(time=time, dt=input$dt, env=env,
             pars_HX=pars_HX, pars_S=pars_S)
    output$table <- renderDataTable(as.data.frame(coral))
    output$growthPlot <- renderPlot(plot(x=time, y=coral$dH.Hdt))
  })
}
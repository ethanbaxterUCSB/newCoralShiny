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
    N <- c(as.numeric(input$N[1])*10^-6, as.numeric(input$N[2])*10^-6, as.numeric(input$Nf))
    X <- c(as.numeric(input$X[1])*10^-7, as.numeric(input$X[2])*10^-7, as.numeric(input$Xf))
    
    #Update inputs
    
    #Create time vector from inputs
    time <- seq(0,input$modelLength,input$dt)
    
    #Create initial environment
    env <- environment(time=time, L=L, N=N, X=X)
    
    #Run model
    vec <- 3:length(time)
    coral <- runCoral(time=time, env=env, pars_HX=pars_HX, pars_S=pars_S)
    output$table <- renderDataTable(as.data.frame(coral))
    output$jLPlot <- renderPlot(plot(x=time[vec], y=coral$j_L[vec], "l"))
    output$dH.HdtPlot <- renderPlot(plot(x=time[vec], y=coral$dH.Hdt[vec], "l"))
    output$dS.SdtPlot <- renderPlot(plot(x=time[vec], y=coral$dS.Sdt[vec], "l"))
    output$Hplot <- renderPlot(plot(x=time[vec], y=coral$H[vec], "l"))
    output$S.tPlot <- renderPlot(plot(x=time[vec], y=coral$S.t[vec], "l"))
    output$SPlot <- renderPlot(plot(x=time[vec], y=coral$S[vec], "l"))
    output$ratioPlot <- renderPlot(plot(x=time[vec], y=coral$HS[vec], "l"))
    output$LPlot <- renderPlot(plot(x=time[vec], y=env$L[vec], "l", col="yellow"))
    output$NPlot <- renderPlot(plot(x=time[vec], y=env$N[vec], "l", col="green"))
    output$XPlot <- renderPlot(plot(x=time[vec], y=env$X[vec], "l", col="blue"))
  })
}
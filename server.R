library(shiny)
source("defPars.R")
source("runCoral.R")

server <- function(input, output) {
  
  output$welcome <- renderText("Welcome! Please check your parameters and then click Run.")
  
  updateInputsAndRun <- observeEvent(input$run, {
    start <- proc.time()[3]
    #Create environment descriptor lists from inputs
    L <- c(as.numeric(input$L[1]), as.numeric(input$L[2]), as.numeric(input$Lf))
    N <- c(as.numeric(input$N[1])*10^-6, as.numeric(input$N[2])*10^-6, as.numeric(input$Nf))
    X <- c(as.numeric(input$X[1])*10^-7, as.numeric(input$X[2])*10^-7, as.numeric(input$Xf))
    
    #Create time vector from inputs
    time <- seq(0,input$modelLength,input$dt)
    
    #Create initial environment
    env <- environment(time=time, L=L, N=N, X=X)
    
    #Run model
    vec <- 3:length(time)
    coral <- runCoral(time=time, env=env, pars_HX=collectPars()$HX, pars_S=collectPars()$S)
    
    #Create data presentation
    output$dH.HdtPlot <- renderPlot(plot(x=time[vec], y=coral$dH.Hdt[vec], type="l", lwd=2,
                                         main="Host Specific Instantaneous Rate of Change of Biomass", xlab="Time (d)", ylab="dH.Hdt (C-mol H / C-mol H / d)"))
    output$dS.SdtPlot <- renderPlot(plot(x=time[vec], y=coral$dS.Sdt[vec], type="l", lwd=2,
                                         main="Symbiont Specific Instantaneous Rate of Change of Biomass", xlab="Time (d)", ylab="dS.Sdt (C-mol S/ C-mol S / d)"))
    output$HPlot <- renderPlot(plot(x=time[vec], y=coral$H[vec], type="l", lwd=2,
                                    main="Host Biomass", xlab="Time (d)", ylab="H (C-mol H)"))
    output$SPlot <- renderPlot(plot(x=time[vec], y=coral$S[vec], type="l", lwd=2,
                                    main="Symbiont Biomass", xlab="Time (d)", ylab="S (C-mol S)"))
    output$HSPlot <- renderPlot(plot(x=time[vec], y=coral$HS[vec], type="l", lwd=2,
                                     main="Host to Symbiont Biomass Ratio", xlab="Time (d)", ylab="H / S (c-mol H / C-mol S)"))
    output$LPlot <- renderPlot(plot(x=time[vec], y=env$L[vec], type="l", col="yellow", lwd=2,
                                    main="Light", xlab="Time (d)", ylab="L (mol photons)"))
    output$NPlot <- renderPlot(plot(x=time[vec], y=env$N[vec], type="l", col="green", lwd=2,
                                    main="Nitrogen", xlab="Time (d)", ylab="DIN (mol N)"))
    output$XPlot <- renderPlot(plot(x=time[vec], y=env$X[vec], type="l", col="blue", lwd=2,
                                    main="Prey", xlab= "Time (d)", ylab="Prey (mol X)"))
    
    output$time <- renderText(paste("Time to complete (including time to plot): ", proc.time()[3]-start, " seconds"))
  })
  
  collectPars <- function() {
    
    #Create and populate host and symbiont parameter lists
    HX <- list(
      j_HT0 = input$j_HT0,
      n_NH = input$n_NH,
      sigma_NH = input$sigma_NH,
      sigma_CH = input$sigma_CH,
      j_Nm = input$j_Nm,
      j_HGm = input$j_HGm,
      k_CO2 = input$k_CO2,
      K_N = input$K_N,
      H_0 = input$H_0,
      j_eC0 = input$j_eC0,
      j_HG0 = input$j_HG0,
      n_NX = input$n_NX,
      j_Xm = input$j_Xm,
      K_X = input$K_X
    )
    
    S <- list(
      y_C = input$y_C,
      j_ST0 = input$j_ST0,
      n_NS = input$n_NS,
      y_CL = input$y_CL,
      k_NPQ = input$k_NPQ,
      k_ROS = input$k_ROS,
      k = input$k,
      aStar = input$aStar,
      sigma_NS = input$sigma_NS,
      sigma_CS = input$sigma_CS,
      j_CPm = input$j_CPm,
      j_SGm = input$j_SGm,
      S_0 = input$S_0,
      b = input$b
    )
    
    return(list(HX=HX, S=S))
  }
  
}
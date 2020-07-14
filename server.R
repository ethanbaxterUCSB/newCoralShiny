library(shiny)
source("defPars.R")
source("runCoral.R")
source("coralPlots.R")

server <- function(input, output) {
  
  output$welcome <- renderText("Welcome! Please check your parameters and then navigate to the model tab.")
  
  #Generate parameters UI
  output$symbPars <- renderUI({
    outVec <- c()
    
    for(num in 1:input$numSymbionts) {
      outVec <- append(outVec, symbInput(num = num))
    }
    
    return(outVec)
  })
  
  #Returns num symbiont inputs
  symbInput <- function(num) {
    
    S <- defPars_S(1)
    
    inputs <- tagList(renderText(paste("Symbiont", num)), fluidRow(
      
      column(6,
        numericInput(inputId = paste0("y_C", num), label = "y_C", value = S["y_C",1]),
        numericInput(inputId = paste0("j_ST0", num), label = "j_ST0", value = S["j_ST0",1]),
        numericInput(inputId = paste0("n_NS", num), label = "n_NS", value = S["n_NS",1]),
        numericInput(inputId = paste0("y_CL", num), label = "y_CL", value = S["y_CL",1]),
        numericInput(inputId = paste0("k_NPQ", num), label = "k_NPQ", value = S["k_NPQ",1]),
        numericInput(inputId = paste0("k_ROS", num), label = "k_ROS", value = S["k_ROS",1]),
        numericInput(inputId = paste0("k", num), label = "k", value = S["k",1])
      ), column(6,
        numericInput(inputId = paste0("aStar", num), label = "aStar", value = S["aStar",1]),
        numericInput(inputId = paste0("sigma_NS", num), label = "sigma_NS", value = S["sigma_NS",1]),
        numericInput(inputId = paste0("sigma_CS", num), label = "sigma_CS", value = S["sigma_CS",1]),
        numericInput(inputId = paste0("j_CPm", num), label = "j_CPm", value = S["j_CPm",1]),
        numericInput(inputId = paste0("j_SGm", num), label = "j_SGm", value = S["j_SGm",1]),
        numericInput(inputId = paste0("S_0", num), label = "S_0", value = S["S_0",1]),
        numericInput(inputId = paste0("b", num), label = "b", value = S["b",1])
      )
    )
    )
    
    return(inputs)
  }
  
  #Reads input, runs model, and renders output
  updateInputsAndRun <- observeEvent(input$run, {
    start <- proc.time()[3]
    #Create environment descriptor lists from inputs
    L <- c(as.numeric(input$L[1]), as.numeric(input$L[2]), as.numeric(input$Lf))
    N <- c(as.numeric(input$N[1])*10^-7, as.numeric(input$N[2])*10^-6, as.numeric(input$Nf))
    X <- c(as.numeric(input$X[1])*10^-8, as.numeric(input$X[2])*10^-7, as.numeric(input$Xf))
    
    #Create time vector from inputs
    time <- seq(0,input$modelLength,input$dt)
    
    #Create initial environment
    env <- environment(time=time, L=L, N=N, X=X)
    
    #Create pars
    pars <- collectPars(num = input$numSymbionts)
    
    #Run Model
    coral <- runCoral(time = time, env = env, pars_HX = pars$HX, pars_S = pars$S)
    
    #Create data presentation
    vec <- 3:length(time)
    
    varNames <- c("L", "N", "X", "j_X", "j_N", "r_NH", "rho_N", "j_eC", "j_CO2", "j_HG","r_CH", "dH.Hdt", "dH.dt", 
                       "H", "j_L", "j_CP", "j_eL", "j_NPQ", "j_SG", "rho_C", "j_ST", "r_CS", "c_ROS", "dS.Sdt", 
                       "dS.dt", "S", "S.t", "SH", "HS")
    plotFuncNames <- varNames
    plotNamesIn <- varNames
    
    #Format plotFuncNames, plotNamesIn, and plotNamesOut
    for (n in 1:length(varNames)) {
      plotNamesIn[n] <- paste0("plot.", varNames[n])
      plotFuncNames[n] <- paste0("coralPlots.", varNames[n])
    }
    
    #Logical vector of which plots to render
    toPlot <- unlist(lapply(reactiveValuesToList(input)[plotNamesIn], 
                     function(i) {unlist(i)}))
    
    #Get number of plots to be rendered
    numPlots <- {
      n <- 0
      for (bool in toPlot) {
        if (bool) {
          n <- n + 1
        }
      }
      n
    }
    
    if (numPlots > 0) {
    
      #Instert number of plots to UI
      output$plots <- renderUI({
        outList <- lapply(1:numPlots, function(i) {
          plotName <- paste0("plot", i)
          plotOutput(plotName, height = 280, width = 560)
        })
        tagList(outList)
      })
    
      #Shorten function name vector with toPlot
      plotFuncNames <- plotFuncNames[toPlot]
      modifiedNames <- varNames[toPlot]
      
      for (i in 1:numPlots) {
        local({
          myI <- i
          output[[paste0("plot", myI)]] <- renderPlot(do.call(plotFuncNames[myI], list(time, as.matrix(get(modifiedNames[myI], coral)))))
        })
      }
    }
    
    
    
    
    output$time <- renderText(paste("Time to complete (including time to plot): ", proc.time()[3]-start, " seconds"))
  })
  
  collectPars <- function(num) {
    
    #list of input
    inList <- reactiveValuesToList(input)
    
    #Create matrix for S pars
    S <- defPars_S(numSymbs = num)
    
    #Update S with user input
    for(symb in 1:num) {
      S["y_C", symb] <- as.numeric(inList[paste0("y_C", symb)][1])
      S["j_ST0", symb] <- as.numeric(inList[paste0("j_ST0", symb)][1])
      S["n_NS", symb] <- as.numeric(inList[paste0("n_NS", symb)][1])
      S["y_CL", symb] <- as.numeric(inList[paste0("y_CL", symb)][1])
      S["k_NPQ", symb] <- as.numeric(inList[paste0("k_NPQ", symb)][1])
      S["k_ROS", symb] <- as.numeric(inList[paste0("k_ROS", symb)][1])
      S["k", symb] <- as.numeric(inList[paste0("k", symb)][1])
      S["aStar", symb] <- as.numeric(inList[paste0("aStar", symb)][1])
      S["sigma_NS", symb] <- as.numeric(inList[paste0("sigma_NS", symb)][1])
      S["sigma_CS", symb] <- as.numeric(inList[paste0("sigma_CS", symb)][1])
      S["j_CPm", symb] <- as.numeric(inList[paste0("j_CPm", symb)][1])
      S["j_SGm", symb] <- as.numeric(inList[paste0("j_SGm", symb)][1])
      S["S_0", symb] <- as.numeric(inList[paste0("S_0", symb)][1])
      S["b", symb] <- as.numeric(inList[paste0("b", symb)][1])
    }
    
    #Collect HX
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
      y_C = input$y_C,
      n_NX = input$n_NX,
      j_Xm = input$j_Xm,
      K_X = input$K_X
    )
    
    return(list(HX = HX, S = S))
  }
}
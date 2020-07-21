library(shiny)
source("defPars.R")
source("runCoral.R")
source("coralPlots.R")

server <- function(input, output, session) {
  
  output$welcome <- renderText("Welcome! If you would like to do so, see the default example run by pressing the Run button in the upper left corner.")
  
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
    
    inputs <- tagList(h4(paste("Symbiont", num)), fluidRow(
      
      column(6,
        numericInput(inputId = paste0("y_C", num), label = div("y_C", span(helpText("Efficiency of biomass synthesis (no units)", style = "font-size:8pt"))), value = S["y_C",1], step = 0.01),
        numericInput(inputId = paste0("j_ST0", num), label = div("j_ST0", span(helpText("Symbiont biomass turnover (C-mol S/C-mol S/d)", style = "font-size:8pt"))), value = S["j_ST0",1], step = 0.01),
        numericInput(inputId = paste0("n_NS", num), label = div("n_NS", span(helpText("Molar N:C ratio of symbiont biomass (no units)", style = "font-size:8pt"))), value = S["n_NS",1], step = 0.01),
        numericInput(inputId = paste0("y_CL", num), label = div("y_CL", span(helpText("Quantum yield of photosynthesis (mol C/mol photons)", style = "font-size:8pt"))), value = S["y_CL",1], step = 0.01),
        numericInput(inputId = paste0("k_NPQ", num), label = div("k_NPQ", span(helpText("Capacity of symbiont for non-photochemical quenching (mol photons/C-mol S/d)", style = "font-size:8pt"))), value = S["k_NPQ",1], step = 1),
        numericInput(inputId = paste0("k_ROS", num), label = div("k_ROS", span(helpText("Excess photon energy required to double reactive oxygen species production, relative to baseline (mol photons/C-mol S/d)", style = "font-size:8pt"))), value = S["k_ROS",1], step = 1),
        numericInput(inputId = paste0("k", num), label = div("k", span(helpText("Exponent on ROS production (no units)", style = "font-size:8pt"))), value = S["k",1], step = 0.1)
      ), column(6,
        numericInput(inputId = paste0("aStar", num), label = div("aStar", span(helpText("Cross-sectional area of symbiont for photosynthesis (m^2/C-mol S)", style = "font-size:8pt"))), value = S["aStar",1], step = 0.01),
        numericInput(inputId = paste0("sigma_NS", num), label = div("sigma_NS", span(helpText("Proportion of Nitrogen recycled from symbiont biomass (no units)", style = "font-size:8pt"))), value = S["sigma_NS",1], step = 0.01),
        numericInput(inputId = paste0("sigma_CS", num), label = div("sigma_CS", span(helpText("Proportion of metabolic CO2 recycled to photosynthesis (no units)", style = "font-size:8pt"))), value = S["sigma_CS",1], step = 0.01),
        numericInput(inputId = paste0("j_CPm", num), label = div("j_CPm", span(helpText("Maximum specific photosynthesis rate of symbiont (mol C/C-mol S/d)", style = "font-size:8pt"))), value = S["j_CPm",1], step = 0.1),
        numericInput(inputId = paste0("j_SGm", num), label = div("j_SGm", span(helpText("Maximum specific biomass synthesis rate of symbiont (C-mol S/C-mol S/d)", style = "font-size:8pt"))), value = S["j_SGm",1], step = 0.01),
        numericInput(inputId = paste0("S_0", num), label = div("S_0", span(helpText("Initial symbiont biomass value (C-mol S)", style = "font-size:8pt"))), value = S["S_0",1], step = 0.1),
        numericInput(inputId = paste0("b", num), label = div("b", span(helpText("Scaling value for bleaching response (no units)", style = "font-size:8pt"))), value = S["b",1], step = 0.1)
      )
    )
    )
    
    return(inputs)
  }
  
  #Reads input, runs model, and renders output
  updateInputsAndRun <- observeEvent(input$run, {
    withProgress(message = "Initializing Model...", {
      updateTabsetPanel(session = session, inputId = "mainPanel", selected = "results")
    
      start <- proc.time()[3]
    
      #Create environment descriptor lists from inputs
      L <- c(as.numeric(input$L[1]), as.numeric(input$L[2]), as.numeric(input$Lf))
      N <- c(as.numeric(input$N[1])*10^-7, as.numeric(input$N[2])*10^-7, as.numeric(input$Nf))
      X <- c(as.numeric(input$X[1])*10^-8, as.numeric(input$X[2])*10^-8, as.numeric(input$Xf))
    
      #Create time vector from inputs
      time <- seq(0,input$modelLength,input$dt)
    
      #Create initial environment
      env <- environment(time=time, L=L, N=N, X=X)
    
      #Create pars
      pars <- collectPars(num = input$numSymbionts)
    }) 
    withProgress(message = "Running Model...", {
      #Run Model
      coral <- runCoral(time = time, env = env, pars_HX = pars$HX, pars_S = pars$S)
    }) 
    withProgress(message = "Initializing plots...", {
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
    }) 
    withProgress(message = paste("Rendering", numPlots, "plots..."), {
      #Set status to plotting
      output$status <- renderText(paste("Rendering", numPlots, "plots..."))
    
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
      output$status <- renderText(paste("Time to complete (including time to plot): ", proc.time()[3]-start, " seconds"))
    })
  })
  
  output$SSUI <- renderUI(
    tagList(conditionalPanel(condition = "input.enableSS==true", {
      helpText("This tab requires a good understanding of the model to utilize well. Please make sure you have said understanding before using it.")
    }),
    conditionalPanel(condition = "input.enableSS==false", {
      helpText("This tab is locked by default. If you would like to explore the more complex steady state runs, please feel free to reactivate it.")
    }))
  )
  
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
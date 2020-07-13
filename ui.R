library(shiny)
source("defPars.R")
source("server.R")

#Create UI
ui <- fluidPage(
  #Title
  titlePanel("Shiny coRal"),
  
  sidebarLayout(
      
    sidebarPanel(width = 3,
                 
        numericInput(inputId="modelLength", 
                    label="Model Length", defPars_t$length),
                 
                 
        numericInput(inputId="dt",label="dt", value=defPars_t$dt),
                 
                 
        sliderInput(inputId="L", label="Light", value= 
                    defPars_env$L, min=0, max=100),
                 
        radioButtons(inputId="Lf", label="Light Function",
                    choiceValues=list(0,1,2), choiceNames= 
                    list("Min to Max", "Max to Min", "Sinusoid")),
                 
                 
        sliderInput(inputId="N", label="DIN", value= 
                    defPars_env$N, min=0, max=100),
                 
        radioButtons(inputId="Nf", label="DIN Function",
                    choiceValues=list(0,1,2), choiceNames= 
                    list("Min to Max", "Max to Min", "Sinusoid")),
                 
                 
        sliderInput(inputId="X", label="Prey", value=
                    defPars_env$X, min=0, max=100),
                 
        radioButtons(inputId="Xf", label="Prey Function", 
                    choiceValues=list(0,1,2), choiceNames=
                    list("Min to Max", "Max to Min", "Sinusoid"))),
    
        mainPanel(
          tabsetPanel(
        
            tabPanel("Parameters",
              conditionalPanel(condition = "input.run==0", textOutput(outputId="welcome")),
              fluidRow(
                column(6, "Host Parameters",
                  fluidRow(
                    column(6,
                      numericInput(inputId = "j_HT0", label = "j_HT0", value = defPars_HX$j_HT0),
                      numericInput(inputId = "n_NH", label = "n_NH", value = defPars_HX$n_NH),
                      numericInput(inputId = "sigma_NH", label = "sigma_NH", value = defPars_HX$sigma_NH),
                      numericInput(inputId = "sigma_CH", label = "sigma_CH", value = defPars_HX$sigma_CH),
                      numericInput(inputId = "j_Nm", label = "j_Nm", value = defPars_HX$j_Nm),
                      numericInput(inputId = "j_HGm", label = "j_HGm", value = defPars_HX$j_HGm),
                      numericInput(inputId = "k_CO2", label = "k_CO2", value = defPars_HX$k_CO2)
                    ),
                    column(6,
                      numericInput(inputId = "K_N", label = "K_N", value = defPars_HX$K_N),
                      numericInput(inputId = "H_0", label = "H_0", value = defPars_HX$H_0),
                      numericInput(inputId = "j_eC0", label = "j_eC0", value = defPars_HX$j_eC0),
                      numericInput(inputId = "j_HG0", label = "j_HG0", value = defPars_HX$j_HG0),
                      numericInput(inputId = "y_C", label = "y_C", value = defPars_HX$y_C),
                
                      numericInput(inputId = "n_NX", label = "n_NX", value = defPars_HX$n_NX),
                      numericInput(inputId = "j_Xm", label = "j_Xm", value = defPars_HX$j_Xm),
                      numericInput(inputId = "K_X", label = "K_X", value = defPars_HX$K_X)
                    )
                  )
                ),
                column(6, "Symbiont Parameters", 
                       numericInput(inputId="numSymbionts", label="Number of Symbionts", value=1, min = 1),
                       uiOutput("symbPars"))
              )
            ),
            tabPanel("Model",
                     actionButton(inputId="run", label="Run"),
                     textOutput(outputId="time"),
                     fluidRow(
                       column(6,
                              plotOutput(outputId="dH.HdtPlot"),
                              plotOutput(outputId="HPlot")
                       ),
                       column(6,
                              plotOutput(outputId="dS.SdtPlot"),
                              plotOutput(outputId="SPlot")
                       )
                     ),
                     plotOutput(outputId="SHPlot"),
                     fluidRow(
                       column(4, plotOutput(outputId="LPlot")),
                       column(4, plotOutput(outputId="NPlot")),
                       column(4, plotOutput(outputId="XPlot"))
                     )
            )
        )
      )
  )
)
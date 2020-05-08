library(shiny)
source("defPars.R")

#Create UI
ui <- fluidPage(
  #Title
  titlePanel("New Coral Shiny App"),
  sidebarLayout(
    #Run Button
    sidebarPanel(actionButton(inputId="run", label="Run"),
                 numericInput(inputId="modelLength", 
                            label="Model Length", defPars_t$length),
                 
                 
                 numericInput(inputId="dt",label="dt", value=defPars_t$dt),
                 
                 
                 sliderInput(inputId="L", label="Light", value= 
                            c(defPars_env$L, defPars_env$L), min=0, max=100),
                 
                 radioButtons(inputId="Lf", label="Light Function",
                            choiceValues=list(0,1,2), choiceNames= 
                            list("Min to Max", "Max to Min", "Sinusoid")),
                 
                 
                 sliderInput(inputId="N", label="DIN", value= 
                            c(defPars_env$N, defPars_env$N), min=0, max=100),
                 
                 radioButtons(inputId="Nf", label="DIN Function",
                            choiceValues=list(0,1,2), choiceNames= 
                            list("Min to Max", "Max to Min", "Sinusoid")),
                 
                 
                 sliderInput(inputId="X", label="Prey", value=
                            c(defPars_env$X, defPars_env$X), min=0, max=100),
                 
                 radioButtons(inputId="Xf", label="Prey Function", 
                            choiceValues=list(0,1,2), choiceNames=
                            list("Min to Max", "Max to Min", "Sinusoid"))
                 ),
    mainPanel(plotOutput(outputId="growthPlot"),
              dataTableOutput(outputId="table"))
  )
)
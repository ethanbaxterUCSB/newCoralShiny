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
        
        helpText("Model length is measured in days."),
                 
                 
        numericInput(inputId="dt",label="dt", value=defPars_t$dt),
                 
                 
        sliderInput(inputId="L", label="Light", value= 
                    defPars_env$L, min=0, max=100),
        helpText("Light is measured in mol photons."),
                 
        radioButtons(inputId="Lf", label="Light Function",
                    choiceValues=list(0,1,2), choiceNames= 
                    list("Min to Max", "Max to Min", "Sinusoid")),
                 
        sliderInput(inputId="N", label="DIN", value= 
                    defPars_env$N, min=0, max=100),
        helpText("DIN input is multiplied by 1e^-7 in calculations. Units are mol N."),         
        
        radioButtons(inputId="Nf", label="DIN Function",
                    choiceValues=list(0,1,2), choiceNames= 
                    list("Min to Max", "Max to Min", "Sinusoid")),
                 
                 
        sliderInput(inputId="X", label="Prey", value=
                    defPars_env$X, min=0, max=100),
        
        helpText("Prey population input is multiplied by 1e^-8 in calculations. Units are mol Prey."),
                 
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
            tabPanel("Plots",
                     helpText("Selected Plots will be shown in the model tab after the model is run. If no plots are selected, the plots from the previous run will be preserved."),
                     fluidPage(
                       fluidRow(
                         column(3,
                                checkboxInput(inputId = "plot.L", label = "Light", value = F),
                                checkboxInput(inputId = "plot.N", label = "DIN", value = F),
                                checkboxInput(inputId = "plot.X", label = "Prey", value = F),
                                checkboxInput(inputId = "plot.j_X", label = "j_X", value = F),
                                checkboxInput(inputId = "plot.j_N", label = "j_N", value = F),
                                checkboxInput(inputId = "plot.r_NH", label = "r_NH", value = F),
                                checkboxInput(inputId = "plot.rho_N", label = "rho_N", value = F)),
                         column(3,
                                checkboxInput(inputId = "plot.j_eC", label = "j_eC", value = F),
                                checkboxInput(inputId = "plot.j_CO2", label = "j_CO2", value = F),
                                checkboxInput(inputId = "plot.j_HG", label = "j_HG", value = F),
                                checkboxInput(inputId = "plot.r_CH", label = "r_CH", value = F),
                                checkboxInput(inputId = "plot.dH.Hdt", label = "dH.Hdt", value = T),
                                checkboxInput(inputId = "plot.dH.dt", label = "dH.dt", value = F),
                                checkboxInput(inputId = "plot.H", label = "H", value = T)),
                         column(3,
                                checkboxInput(inputId = "plot.j_L", label = "j_L", value = F),
                                checkboxInput(inputId = "plot.j_CP", label = "j_CP", value = F),
                                checkboxInput(inputId = "plot.j_eL", label = "j_eL", value = F),
                                checkboxInput(inputId = "plot.j_NPQ", label = "j_NPQ", value = F),
                                checkboxInput(inputId = "plot.j_SG", label = "j_SG", value = F),
                                checkboxInput(inputId = "plot.rho_C", label = "rho_C", value = F),
                                checkboxInput(inputId = "plot.j_ST", label = "j_ST", value = F)),
                         column(3,
                                checkboxInput(inputId = "plot.r_CS", label = "r_CS", value = F),
                                checkboxInput(inputId = "plot.c_ROS", label = "c_ROS", value = F),
                                checkboxInput(inputId = "plot.dS.Sdt", label = "dS.Sdt", value = T),
                                checkboxInput(inputId = "plot.dS.dt", label = "dS.dt", value = F),
                                checkboxInput(inputId = "plot.S", label = "S", value = T),
                                checkboxInput(inputId = "plot.S.t", label = "S.t", value = F),
                                checkboxInput(inputId = "plot.HS", label = "HS", value = F),
                                checkboxInput(inputId = "plot.SH", label = "SH", value = T))
                     )
                  )
            ),
            tabPanel("Model",
                     actionButton(inputId="run", label="Run"),
                     textOutput(outputId="time"),
                     uiOutput("plots")
            )
        )
      )
  )
)
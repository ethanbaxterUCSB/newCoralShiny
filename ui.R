library(shiny)
source("defPars.R")
source("server.R")

#Create UI
ui <- fluidPage(
  #Title
  titlePanel("Shiny coRal"),
  
  sidebarLayout(
      
    sidebarPanel(width = 3,
        actionButton(inputId="run", label="Run"),         
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
          tabsetPanel(id = "tabs",
        
            tabPanel("Parameters",
              conditionalPanel(condition = "input.run==0", textOutput(outputId="welcome")),
              fluidRow(
                column(6, h3("Host Parameters"),
                  fluidRow(
                    column(6,
                      numericInput(inputId = "j_HT0", label = div("j_HT0", span(helpText("Host biomass turnover rate (1/d)", style = "font-size:8pt"))), value = defPars_HX$j_HT0, step = 0.01),
                      numericInput(inputId = "n_NH", label = div("n_NH", span(helpText("Molar N:C ratio in host biomass (no units)", style = "font-size:8pt"))), value = defPars_HX$n_NH, step = 0.01),
                      numericInput(inputId = "sigma_NH", label = div("sigma_NH", span(helpText("Proportion of Nitrogen recycled from host biomass (no units)", style = "font-size:8pt"))), value = defPars_HX$sigma_NH, step = 0.01),
                      numericInput(inputId = "sigma_CH", label = div("sigma_CH", span(helpText("Proportion of metabolic CO2 recycled to photosynthesis (no units)", style = "font-size:8pt"))), value = defPars_HX$sigma_CH, step = 0.01),
                      numericInput(inputId = "j_Nm", label = div("j_Nm", span(helpText("Maximum uptake of DIN into host (mol N/C-mol H/d)", style = "font-size:8pt"))), value = defPars_HX$j_Nm, step = 0.001),
                      numericInput(inputId = "j_HGm", label = div("j_HGm", span(helpText("Maximum specific biomass synthesis of host (C-mol H/C-mol H/d)", style = "font-size:8pt"))), value = defPars_HX$j_HGm, step = 0.1),
                      numericInput(inputId = "k_CO2", label = div("k_CO2", span(helpText("Efficacy of CO2 transfer to symbiont photosynthesis (mol CO2/mol C)", style = "font-size:8pt"))), value = defPars_HX$k_CO2, step = 0.1)
                    ),
                    column(6,
                      numericInput(inputId = "K_N", label = div("K_N", span(helpText("Half-Saturation constant for host DIN uptake (mol N/L)", style = "font-size:8pt"))), value = defPars_HX$K_N, step = 1e-7),
                      numericInput(inputId = "H_0", label = div("H_0", span(helpText("Initial host biomass (C-mol H)", style = "font-size:8pt"))), value = defPars_HX$H_0, step = 0.1),
                      numericInput(inputId = "j_eC0", label = div("j_eC0", span(helpText("Initial excess Carbon flux to symbiont(s) (mol C/C-mol H/d)", style = "font-size:8pt"))), value = defPars_HX$j_eC0, step = 0.1),
                      numericInput(inputId = "j_HG0", label = div("j_HG0", span(helpText("Initial host specific biomass synthesis rate (C-mol H/C-mol H/d)", style = "font-size:8pt"))), value = defPars_HX$j_HG0, step = 0.01),
                      numericInput(inputId = "y_C", label = div("y_C", span(helpText("Efficiency of biomass synthesis (C-mol/mol C)", style = "font-size:8pt"))), value = defPars_HX$y_C, step = 0.01),
                
                      numericInput(inputId = "n_NX", label = div("n_NX", span(helpText("Molar N:C ratio in prey biomass (no units)", style = "font-size:8pt"))), value = defPars_HX$n_NX, step = 0.1),
                      numericInput(inputId = "j_Xm", label = div("j_Xm", span(helpText("Maximum uptake of prey into host (C-mol X/C-mol H/d)", style = "font-size:8pt"))), value = defPars_HX$j_Xm, step = 0.01),
                      numericInput(inputId = "K_X", label = div("K_X", span(helpText("Half saturation constant for host uptake of prey (C-mol X/L)", style = "font-size:8pt"))), value = defPars_HX$K_X, step = 1e-7)
                    )
                  )
                ),
                column(6, h3("Symbiont Parameters"), 
                       numericInput(inputId="numSymbionts", label=div("Number of Symbionts", span(helpText("The recommended maximum for symbionts is eight.", style = "font-size:8pt"))), value=1, min = 1),
                       uiOutput("symbPars"))
              ),
              checkboxInput(inputId="enableSS", label="Enable Steady State Tab", value = F)
            ),
            tabPanel("Select Plots",
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
            tabPanel("View Results", value = "results",
                     textOutput(outputId="status"),
                     uiOutput("plots")
            ),
            tabPanel("Information",
                     fluidPage(
                       column(12,
                              h3("Model Information"),
                              p("The model that this shiny app is based off of is described in the paper \"A Dynamic Bioenergetic Model for Cora-Symbiodinium Symbioses and Coral Bleaching as an Alternate Stable State\" by Ross Cunning, Erik B. Muller, Ruth D. Gates, and Roger M. Nisbet. The paper can be found at the following web address."),
                              div("(1): ", a("https://doi.org/10.1016/j.jtbi.2017.08.003", href = "https://doi.org/10.1016/j.jtbi.2017.08.003")),
                              h5(), #For spacing
                              p("It is recommended to have read at least the abstract of the paper, which follows later. Also, if you would like to interact directly with the model in R, the coRal R package is on gitHub."),
                              div("(2): ", a("https://github.com/jrcunning/coRal", href = "https://github.com/jrcunning/coRal")),
                              h5(), #spacing
                              p("The code for the shiny app is also available on gitHub."),
                              div("(3): ", a("https://github.com/ethanbaxterUCSB/newCoralShiny", href = "https://github.com/ethanbaxterUCSB/newCoralShiny")),
                              h4(), #spacing
                              h4("Paper Abstract"),
                              h6("(Taken directly from link 1 above.)"),
                              p("Coral reef ecosystems owe their ecological success – and vulnerability to climate change – to the symbiotic metabolism of corals and Symbiodinium spp. The urgency to understand and predict the stability and breakdown of these symbioses (i.e., coral ‘bleaching’) demands the development and application of theoretical tools. Here, we develop a dynamic bioenergetic model of coral-Symbiodinium symbioses that demonstrates realistic steady-state patterns in coral growth and symbiont abundance across gradients of light, nutrients, and feeding. Furthermore, by including a mechanistic treatment of photo-oxidative stress, the model displays dynamics of bleaching and recovery that can be explained as transitions between alternate stable states. These dynamics reveal that “healthy” and “bleached” states correspond broadly to nitrogen- and carbon-limitation in the system, with transitions between them occurring as integrated responses to multiple environmental factors. Indeed, a suite of complex emergent behaviors reproduced by the model (e.g., bleaching is exacerbated by nutrients and attenuated by feeding) suggests it captures many important attributes of the system; meanwhile, its modular framework and open source R code are designed to facilitate further problem-specific development. We see significant potential for this modeling framework to generate testable hypotheses and predict integrated, mechanistic responses of corals to environmental change, with important implications for understanding the performance and maintenance of symbiotic systems."),
                              h4("Graphic"),
                              p("Below is Figure 1 (and its description) from the mentioned paper. It is quite intuitive and may serve as a handy tool for exploration of the model and this shiny app."),
                              img(src = "fig1-with-description.png"),
                              h4("About the Developers"),
                              p("This shiny app was developed by Ethan Baxter, under the mentorship of Drs. Alexandra Brown and Ferdinand Pfab, while working in the Moeller Lab at the University of California, Santa Barbara."),
                              uiOutput("contactForm")
                       )
                     )
            ),
            tabPanel("Steady State", value = "SSTab",
                     uiOutput("SSUI"))
        )
      )
  ),
  helpText("Last updated 07/24/2020 by Ethan Baxter, UCSB undergraduate, Moeller Lab. Have a great day!")
)
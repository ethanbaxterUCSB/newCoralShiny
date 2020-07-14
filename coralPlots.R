
#Easier way to do the x-axis label
xScript <- "Time (d)"

#Gives the necessary y-axis range for a plot, assuming user wants to see all points.
#(Only useful for multi-line plots)
dataRange <- function(myMatrix) {c(min(myMatrix), max(myMatrix))}

#Generates a single-line plot from a list
singlePlot <- function(time, myList, main, ylab, col = 1) {
  plot(x = time, y = myList, type = "l", lwd = 2, main = main, xlab = xScript,
       ylab = ylab, col = col)
}

#Generates a multi-line plot from a matrix, assumes ncol(myMat) == length(time)
multiPlot <- function(time, myMat, main, ylab) {
  plot(x = time, y = myMat[,1], type = "l", lwd = 2, main = main, xlab = xScript, ylab = ylab,
       ylim = dataRange(myMat))
  if(ncol(myMat) > 1) {
    text(x = time[length(time)/2], y = dataRange(myMat)[2], labels = "Symbiont 1", col = 1, pos = 1, offset = 0)
    for (s in 2:ncol(myMat)) {
      lines(x = time, y = myMat[,s], col = s, lwd = 2)
      text(x = time[length(time)/2], y = dataRange(myMat)[2], labels = paste("Symbiont", s), col = s, pos = 1, offset = s-1)
    }
  }
  
}

#Begin Plots

#Single-line Plots

coralPlots.L <- function(time, L) {singlePlot(time = time, 
                                              myList = L, 
                                              main = "Light", 
                                              ylab = "L (mol photons)", 
                                              col = "yellow")}

coralPlots.N <- function(time, N) {singlePlot(time = time, 
                                              myList = N, 
                                              main = "Nitrogen", 
                                              ylab = "DIN (mol N)", 
                                              col = "green")}

coralPlots.X <- function(time, X) {singlePlot(time = time, 
                                              myList = X, 
                                              main = "Prey",
                                              ylab = "Prey (mol X)",
                                              col = "blue")}

coralPlots.j_X <- function(time, j_X) {singlePlot(time = time, 
                                                  myList = j_X, 
                                                  main = "Uptake of Prey", 
                                                  ylab = "j_X (C-mol X / C-mol H / d)")}

coralPlots.j_N <- function(time, j_N) {singlePlot(time = time, 
                                                  myList = j_N, 
                                                  main = "Uptake of DIN", 
                                                  ylab = "j_N (mol N / C-mol H / d)")} 

coralPlots.r_NH <- function(time, r_NH) {singlePlot(time = time, 
                                                    myList = r_NH, 
                                                    main = "Recycled Nitrogen from Host Turnover", 
                                                    ylab = "r_NH (mol N / C-mol H / d)")}

coralPlots.rho_N <- function(time, rho_N) {singlePlot(time = time, 
                                                      myList = rho_N,
                                                      main = "Nitrogen Shared with Symbionts",
                                                      ylab = "rho_N (mol N / C-mol H / d")}

coralPlots.j_eC <- function(time, j_eC) {singlePlot(time = time, 
                                                    myList = j_eC, 
                                                    main = "Excess Carbon Used to Activate Host CCMs", 
                                                    ylab = "j_eC (mol C / C-mol H / d)")}

coralPlots.j_CO2 <- function(time, j_CO2) {singlePlot(time = time, 
                                                      myList = j_CO2, 
                                                      main = "CO2 Input to Photosynthesis", 
                                                      ylab = "j_CO2 (mol CO2 / C-mol H / d")}

coralPlots.j_HG <- function(time, j_HG) {singlePlot(time = time, 
                                                    myList = j_HG, 
                                                    main = "Host Biomass Formation Rate", 
                                                    ylab = "j_HG (C-mol H / C-mol H / d)")}

coralPlots.r_CH <- function(time, r_CH) {singlePlot(time = time, 
                                                    myList = r_CH, 
                                                    main = "Recycled CO2 from Host", 
                                                    ylab = "r_CH (mol CO2 / C-mol H / d)")}

coralPlots.dH.Hdt <- function(time, dH.Hdt) {singlePlot(time = time, 
                                                        myList = dH.Hdt, 
                                                        main = "Specific Host Growth Rate", 
                                                        ylab = "dH.Hdt (C-mol H / C-mol H / d)")}

coralPlots.dH.dt <- function(time, dH.dt) {singlePlot(time = time, 
                                                      myList = dH.dt, 
                                                      main = "Host Growth Rate", 
                                                      ylab = "dH.dt (C-mol H / d)")}

coralPlots.H <- function(time, H) {singlePlot(time = time, 
                                              myList = H, 
                                              main = "Host Biomass", 
                                              ylab = "H (C-mol H)")}

coralPlots.HS <- function(time, HS) {singlePlot(time = time, 
                                               myList = HS, 
                                               main = "Host to Total Symbiont Biomass Ratio", 
                                               ylab = "H / S.t (C-mol H / C-mol S)")}

coralPlots.SH <- function(time, SH) {singlePlot(time = time, 
                                               myList = SH, 
                                               main = "Total Symbiont to Host Biomass Ratio", 
                                               ylab = "S.t / H (C-mol S / C-mol H)")}

coralPlots.S.t <- function(time, S.t) {singlePlot(time = time,
                                                 myList = S.t,
                                                 main = "Total Symbiont Biomass",
                                                 ylab = "S.t (C-mol S)")}


#End Single-line plots
#
#
#Multi-line plots

coralPlots.j_L <- function(time, j_L) {multiPlot(time = time, 
                                                 myMat = j_L, 
                                                 main = "Light Absorption Rate", 
                                                 ylab = "j_L (mol photons / C-mol S / d)")}

coralPlots.j_CP <- function(time, j_CP) {multiPlot(time = time,
                                                   myMat = j_CP,
                                                   main = "Photosynthesis Rate",
                                                   ylab = "j_CP (mol C / C-mol S / d)")}

coralPlots.j_eL <- function(time, j_eL) {multiPlot(time = time,
                                                   myMat = j_eL,
                                                   main = "Excess Light Energy",
                                                   ylab = "j_eL (mol photons / C-mol S / d)")}

coralPlots.j_NPQ <- function(time, j_NPQ) {multiPlot(time = time,
                                                     myMat = j_NPQ,
                                                     main = "Total Capacity of Non-Photochemical Quenching",
                                                     ylab = "j_NPQ (mol photons / C-mol S / d)")}

coralPlots.j_SG <- function(time, j_SG) {multiPlot(time = time,
                                                   myMat = j_SG,
                                                   main = "Symbiont Biomass Formation Rate",
                                                   ylab = "j_SG (C-mol S / C-mol S / d")}

coralPlots.rho_C <- function(time, rho_C) {multiPlot(time = time,
                                                     myMat = rho_C,
                                                     main = "Fixed Carbon Shared with Host",
                                                     ylab = "rho_C (mol C / C-mol S / d)")}

coralPlots.j_ST <- function(time, j_ST) {multiPlot(time = time,
                                                   myMat = j_ST,
                                                   main = "Symbiont Biomass Turnover Rate",
                                                   ylab = "j_ST (C-mol S / C-mol S / d)")}

coralPlots.r_CS <- function(time, r_CS) {multiPlot(time = time,
                                                   myMat = r_CS,
                                                   main = "Recycled CO2 from Symbiont",
                                                   ylab = "r_CS (mol CO2 / C-mol S / d)")}

coralPlots.c_ROS <- function(time, c_ROS) {multiPlot(time = time,
                                                     myMat = c_ROS,
                                                     main = "ROS Production Proportional to Baseline",
                                                     ylab = "c_ROS (-)")}

coralPlots.dS.Sdt <- function(time, dS.Sdt) {multiPlot(time = time,
                                                       myMat = dS.Sdt,
                                                       main = "Specific Symbiont Growth Rate",
                                                       ylab = "dS.Sdt (C-mol S / C-mol S / d)")}

coralPlots.dS.dt <- function(time, dS.dt) {multiPlot(time = time,
                                                     myMat = dS.dt,
                                                     main = "Symbiont Growth Rate",
                                                     ylab = "dS.dt (C-mol S / d)")}

coralPlots.S <- function(time, S) {multiPlot(time = time,
                                             myMat = S,
                                             main = "Symbiont Biomass",
                                             ylab = "S (C-mol S)")}


#'This is basically the same as runCoral, except the model runs until either
#'a steady state is reached or it is confirmed to be oscillating at t=1000.
#'Requires a constant environment.

library(shiny)
source("runCoral.R") #Gotta reuse those functions :) (will be using synth, MMk, alwaysPositive, v.synth, and vectorized versions)

#A single steady state run
runCoralSS <- function(env, pars_HX, pars_S) {
  
  #----------------
  #Define variables
  #----------------
  
  #Numeric
  nSymb <- as.numeric(ncol(pars_S)) #The number of symbionts in the model
  dt <- 0.1 #Default dt
  sensitivity <- 0.00001 #How close the compared steps need to be
  
  #Single Column
  for(var in c("time", "rho_N", "j_eC", "j_CO2", "j_HG","r_CH", "dH.Hdt", "dH.dt", "H", "SH", "HS", "S.t")){
    assign(var, list())
  }
  
  #Single Column, repeated values
  j_X <- list(MMk(substrate = env["X"], max = pars_HX$j_Xm, halfSat = pars_HX$K_X))
  j_N <- list(MMk(substrate = env["N"], max = pars_HX$j_Nm, halfSat = pars_HX$K_N)) 
  r_NH <- list((pars_HX$j_HT0 * pars_HX$n_NH * pars_HX$sigma_NH))
  
  #Multiple Columns
  for (var in c("j_L", "j_CP", "j_eL", "j_NPQ", "j_SG", "rho_C", "j_ST", "r_CS", "c_ROS", "dS.Sdt", "dS.dt", "S")) {
    assign(var, as.data.frame(matrix(ncol = nSymb)))
  }
  
  #Multiple Columns, repeated values
  r_NS <- as.data.frame(matrix(pars_S["j_ST0",] * pars_S["n_NS",] * pars_S["sigma_NS",], ncol = nSymb))
  
  #------------------
  #Set initial values
  #------------------
  
  t0 <- 1
  
  time[t0] <- 0
  
  #Host
  rho_N[t0] <- j_N[[1]] 
  j_eC[t0] <- pars_HX$j_eC0 
  j_CO2[t0] <- pars_HX$k_CO2 * j_eC[[1]]
  j_HG[t0] <- pars_HX$j_HG0
  r_CH[t0] <- pars_HX$j_HT0 * pars_HX$sigma_CH
  dH.Hdt[t0] <- pars_HX$j_HGm
  H[t0] <- pars_HX$H_0
  dH.dt[t0] <- dH.Hdt[[1]] * H[[1]]
  
  #Symbiont
  j_L[t0,] <- env["L"] * pars_S["aStar",]
  j_CP[t0,] <- v.alwaysPositive(rejFlux = (v.synth(substrateOne = j_L[1,] * pars_S["y_CL",], substrateTwo = j_CO2[[1]] * H[[1]] / pars_S["S_0",], max = pars_S["j_CPm",])))
  j_eL[t0,] <- v.alwaysPositive(rejFlux=(j_L[1,] - j_CP[1,] / pars_S["y_CL",]))
  j_NPQ[t0,] <- pars_S["k_NPQ",]
  j_SG[t0,] <- pars_S["j_SGm",] / 10
  rho_C[t0,] <- j_CP[1,]
  j_ST[t0,] <- pars_S["j_ST0",]
  r_CS[t0,] <- pars_S["j_ST0",] * pars_S["sigma_CS",]
  c_ROS[t0,] <- 1
  dS.Sdt[t0,] <- pars_S["j_SGm",]
  S[t0,] <- pars_S["S_0",]
  dS.dt[t0,] <- dS.Sdt[1,] * S[1,]
  S.t[t0] <- sum(S[1,])
  HS[t0] <- H[[1]]/S.t[[1]]
  SH[t0] <- S.t[[1]]/H[[1]]
  
  
  #-------------
  #Run the model
  #-------------
  
  #While not at a steady state (S:H or Host Growth) and not oscillating past t=1000
  SHSS <- FALSE #S:H Steady State boolean
  HGSS <- FALSE #Host Growth Steady State boolean
  checkIndex <- 0
  t <- 2 #This is different from time (the vector), consider shorthand for step number where the initial step is step 1
  while (!SHSS && !HGSS) {
    
    #Run current time step
    #---------------------
    
    #Set time
    time[t] <- dt * (t - 1)
    
    #Add length to repeated values
    j_X[t] <- j_X[[t-1]]
    j_N[t] <- j_N[[t-1]]
    r_NH[t] <- r_NH[[t-1]]
    r_NS[t,] <- r_NS[t-1,]
    
    #Symbiont
    S.t[t] <- sum(S[t-1,])
    
    j_L[t,] <- (1.256307 + 1.385969 * exp(-6.479055 * S.t[[t]]/H[[t-1]])) * env["L"] * pars_S["aStar",]
    r_CS[t,] <- pars_S["sigma_CS",] * (pars_S["j_ST0",] + (1-pars_S["y_C",])*j_SG[t-1,]/pars_S["y_C",])
    j_CP[t,] <- v.synth(substrateOne = (j_L[t,] * pars_S["y_CL",]), substrateTwo = ((j_CO2[[t-1]] + r_CH[[t-1]])*H[[t-1]]/S.t[[t]] + r_CS[t,]), max = (pars_S["j_CPm",])) / c_ROS[t-1,]
    j_eL[t,] <- v.alwaysPositive(rejFlux = (j_L[t,] - j_CP[t,]/pars_S["y_CL",]))
    j_NPQ[t,] <- (pars_S["k_NPQ",]^(-1)+j_eL[t,]^(-1))^(-1)
    c_ROS[t,] <- 1 + (v.alwaysPositive(rejFlux = j_eL[t,] - j_NPQ[t,]) / pars_S["k_ROS",])^pars_S["k",]
    j_SG[t,] <- v.synth(substrateOne = (pars_S["y_C",]*j_CP[t,]), substrateTwo = ((rho_N[[t-1]]*H[[t-1]]/S.t[[t]] + r_NS[t,])/pars_S["n_NS",]), max = (pars_S["j_SGm",]))
    rho_C[t,] <- v.alwaysPositive(rejFlux = (j_CP[t,] - j_SG[t,]/pars_S["y_C",]))
    j_ST[t,] <- pars_S["j_ST0",] * (1 + pars_S["b",] * (c_ROS[t,] - 1)) 
    
    dS.Sdt[t,] <- j_SG[t,] - j_ST[t,] 
    dS.dt[t,] <- dS.Sdt[t,] * S[t-1,] 
    S[t,] <- S[t-1,] + dS.dt[t,] * dt 
    
    rho_C.t <- sum(rho_C[t,]*S[t-1,]) 
    
    #Host
    j_HG[t] <- synth(substrateOne = (pars_HX$y_C * (rho_C.t/H[[t-1]] + j_X[[t]])), substrateTwo = ((j_N[[t]] + pars_HX$n_NX * j_X[[t]] + r_NH[[t]]) / pars_HX$n_NH), max = (pars_HX$j_HGm)) #Host biomass production rate
    rho_N[t] <- alwaysPositive(j_N[[t]] + pars_HX$n_NX * j_X[[t]] + r_NH[[t]] - pars_HX$n_NH * j_HG[[t]] / pars_HX$y_C) 
    j_eC[t] <- alwaysPositive(j_X[[t]] + rho_C.t/H[[t-1]] - j_HG[[t]] / pars_HX$y_C)
    r_CH[t] <- pars_HX$sigma_CH * (pars_HX$j_HT0 + (1-pars_HX$y_C) * j_HG[[t]] / pars_HX$y_C)
    j_CO2[t] <- pars_HX$k_CO2 * j_eC[[t]]
    
    dH.Hdt[t] <- j_HG[[t]] - pars_HX$j_HT0
    dH.dt[t] <- dH.Hdt[[t]] * H[[t-1]] 
    H[t] <- H[[t-1]] + dH.dt[[t]] * dt 
    
    HS[t] <- H[[t]]/S.t[[t]]
    SH[t] <- S.t[[t]]/H[[t]]
    
    #Check for steady states and oscillation
    #---------------------------------------
    toCheckSS <- t > 20 / dt
    
    checkIndex <- ifelse(toCheckSS, t - 10 / dt, 0) #index of step to check against
    HGSS <- ifelse(toCheckSS, abs(dH.Hdt[[t]] - dH.Hdt[[checkIndex]]) <= sensitivity, F) #if less than or equal to sensitivity, host growth steady state has been reached
    SHSS <- ifelse(toCheckSS, abs(SH[[t]] - SH[[checkIndex]]) <= sensitivity, F) #if less than or equal to sensitivity, symbiont:host steady state has been reached
    #Check if oscillating if t>1000
    if(t > 1000) {
      #sum(diff(sign(diffS.t / H))!=0) is the sum of how many times dSH.dt changes sign, uses trick where numeric operators will read TRUE as 1 and FALSE as 0
      bool <- sum(diff(sign(diff(unlist(SH))))!=0) >= 3
      if (bool) {
        #If system is oscillating, mark as both HGSS and SHSS with host growth as 0
        HGSS <- TRUE
        SHSS <- TRUE
        dH.Hdt[t] <- 0
      }
    }
    
    #Update t
    t <- t + 1
    
  }
  
  #Return a bunch of stuff, the same stuff as runCoral
  return(list(time = time, L=env["L"], N=env["N"], X=env["X"], j_X=j_X, j_N=j_N, r_NH=r_NH, rho_N=rho_N, j_eC=j_eC, j_CO2=j_CO2, 
              j_HG=j_HG, r_CH=r_CH, dH.Hdt=dH.Hdt, dH.dt=dH.dt, H=H, r_NS=r_NS, j_L=j_L, 
              j_CP=j_CP, j_eL=j_eL, j_NPQ=j_NPQ, j_SG=j_SG, rho_C=rho_C, j_ST=j_ST, 
              r_CS=r_CS, c_ROS=c_ROS, dS.Sdt=dS.Sdt, dS.dt=dS.dt, S=S, HS=HS, SH=SH, S.t=S.t, SHSS = SHSS, HGSS = HGSS))
  
}

#Express multiple runs
runCoralSS_multiple <- function(xAxis, xRange, xRes, yAxis, yRange, yRes, env, pars_HX, pars_S) {
  X <- seq(xRange[1], xRange[2], xRes)
  Y <- seq(yRange[1], yRange[2], yRes)
  
  for (x in X) {
    c(env, pars_HX, pars_S)[xAxis]
  }
}
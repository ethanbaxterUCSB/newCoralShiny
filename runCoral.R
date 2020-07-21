library(shiny)

#Function synth returns the production rate of biomass given input rates of 
#two substrates substrateOne and substrateTwo as well as a maximum flux
synth <- function(substrateOne, substrateTwo, max) {
  A <- as.numeric(substrateOne)
  B <- as.numeric(substrateTwo)
  max <- as.numeric(max)
  
  if (A==0 && B==0) {return(0)}
  
  val <- (A * B * (A + B) * max)/(A^2 * B + A * B^2 + A^2 * max + A * B * max + B^2 * max)
  #val <- 1 / ((1 / max) + (1 / A) + (1 / B) - (1 / (A + B)))
  
  return(val)
}

#Function MMk is a generic Michaelis-Menten kinetics model that returns the
#assimilation rate of the given substrate
MMk <- function(substrate, max, halfSat) {
  substrate <- as.numeric(substrate)
  max <- as.numeric(max)
  halfSat <- as.numeric(halfSat)
  
  return((max*substrate)/(substrate+halfSat))
}

#Function alwaysPositive returns 0 if the given rejection flux rejFlux is
#negative and rejflux if it is greater than zero because rejection fluxes 
#must always be positive
alwaysPositive <- function(rejFlux) {
  rejFlux <- as.numeric(rejFlux)
  
  return(max(0, rejFlux))
}

#Vectorized functions, allows for use with multiple symbionts 
#(MMk does not need to be vectorized)
v.synth <- Vectorize(synth)
v.alwaysPositive <- Vectorize(alwaysPositive)

#Function environment returns the initial environment given input lists time,
#L, N, and X that each specify the behavior of the aspects of the environment 
#over time
environment <- function(time, L, N, X) {
  const1=as.numeric(0.0172)
  
  # Light
  outL <- if (L[3]==0) {
    seq(L[1], L[2], along.with = time) #min to max
    
  } else if (L[3]==1) {
    seq(L[2], L[1], along.with = time) #max to min
    
  } else {
    0.5 * (L[2] - L[1]) * sin(const1*time) + L[1] + 0.5 * (L[2] - L[1]) #sinusoid
    
  }
  
  # DIN
  outN <- if (N[3]==0) {
    seq(N[1], N[2], along.with = time) #min to max
    
  } else if (N[3]==1) {
    seq(N[2], N[1], along.with = time) #max to min
    
  } else {
    0.5 * (N[2] - N[1]) * sin(const1*time) + N[1] + 0.5 * (N[2] - N[1]) #sinusoid
    
  }
  
  # Prey
  outX <- if (X[3]==0) {
    seq(X[1], X[2], along.with = time) #min to max
    
  } else if (X[3]==1) {
    seq(X[2], X[1], along.with = time) #max to min
    
  } else {
    0.5 * (X[2] - X[1]) * sin(const1*time) + 0.5 * (X[2] - X[1]) #sinusoid
    
  }
  
  # Set environment specifications
  env <- list(L=outL, N=outN, X=outX)
  return(env)
}


#Function runCoral models the growth of symbiont and host biomasses over the
#given time interval, and returns a list of vectors that describe the
#state at a given time
runCoral <- function(time, env, pars_HX, pars_S) {
  
  nSymb <- as.numeric(ncol(pars_S))
  
  #Create lists
  for(var in c("rho_N", "j_eC", "j_CO2", "j_HG","r_CH", "dH.Hdt", "dH.dt", "H", "SH", "HS", "S.t")){
    assign(var, rep(NA, times=length(time)))
  }
  #Create matrices
  for (var in c("j_L", "j_CP", "j_eL", "j_NPQ", "j_SG", "rho_C", "j_ST", "r_CS", "c_ROS", "dS.Sdt", "dS.dt", "S")) {
    assign(var, matrix(nrow = length(time), ncol = nSymb))
  }
  
  #Initial Host fluxes
  j_X <- rep(x = MMk(substrate = env$X[1], max = pars_HX$j_Xm, halfSat = pars_HX$K_X), times = length(time)) #Uptake of prey
  j_N <- rep(x = MMk(substrate = env$N[1], max = pars_HX$j_Nm, halfSat = pars_HX$K_N), times = length(time)) #Uptake of DIN
  
  r_NH <- rep(x = (pars_HX$j_HT0 * pars_HX$n_NH * pars_HX$sigma_NH), times = length(time)) #Recycled Nitrogen from host turnover
  rho_N[1] <- j_N[1] #Nitrogen shared with the Symbiont
  j_eC[1] <- pars_HX$j_eC0 #Excess carbon used to activate host CCMs
  j_CO2[1] <- pars_HX$k_CO2 * j_eC[1] #Uptake of CO2
  j_HG <- rep(x = pars_HX$j_HG0, times = length(time)) #Host biomass growth rate
  r_CH[1] <- pars_HX$j_HT0 * pars_HX$sigma_CH #Recycled CO2 from host
  
  dH.Hdt[1] <- pars_HX$j_HGm #Specific host instantaneous growth rate
  H[1] <- pars_HX$H_0 #Initial host biomass
  dH.dt[1] <- dH.Hdt[1] * H[1] #Initial host instantaneous growth rate
  
  #Initial Symbiont fluxes
  r_NS <- matrix(pars_S["j_ST0",] * pars_S["n_NS",] * pars_S["sigma_NS",], nrow = length(time), ncol = nSymb) #Recycled Nitrogen from Symbiont turnover
  
  j_L[1,] <- env$L[1] * pars_S["aStar",] #Initial uptake of light
  j_CP[1,] <- v.alwaysPositive(rejFlux=(v.synth(substrateOne = j_L[1,] * pars_S["y_CL",], substrateTwo = j_CO2[1]*H[1]/pars_S["S_0",], max = pars_S["j_CPm",]))) #Initial production of biomass from photosynthesis
  j_eL[1,] <- v.alwaysPositive(rejFlux=(j_L[1,] - j_CP[1,]/pars_S["y_CL",])) #Excess light energy in the system
  j_NPQ[1,] <- pars_S["k_NPQ",] #Total capacity of nonphotochemical quenching
  j_SG[1,] <- pars_S["j_SGm",]/10 #Production of Symbiont biomass
  rho_C[1,] <- j_CP[1,] #Fixed carbon shared with host
  j_ST[1,] <- pars_S["j_ST0",] #Initial biomass turnover rate
  r_CS[1,] <- pars_S["j_ST0",] * pars_S["sigma_CS",] #Recycled CO2 from symbiont
  c_ROS[1,] <- 1 #Reactive oxygen species production relative to baseline
  
  dS.Sdt[1,] <- pars_S["j_SGm",] #Symbiont specific instantaneus growth rate
  S[1,] <- pars_S["S_0",] #Initial Symbiont biomass
  dS.dt[1,] <- dS.Sdt[1,] * S[1,]  #initial symbiont instantaneous growth rate
  
  S.t[1] <- sum(S[1,]) #Initial total symbiont biomass
  HS[1] <- H[1]/S.t[1] #Initial host to symbiont biomass ratio
  SH[1] <- S.t[1]/H[1] #Initial symbiont to host biomass ratio
  
  dt <- time[2]-time[1] #timestep size
  
  #Run Model
  
  for (t in (2:length(time))) {
    
    #Symbiont
    #--------
    S.t[t] <- sum(S[t-1,])  #Total Symbiont biomass
    
    j_L[t,] <- (1.256307 + 1.385969 * exp(-6.479055 * S.t[t]/H[t-1])) * env$L[t] * pars_S["aStar",] #New uptake of light into symbiont
    r_CS[t,] <- pars_S["sigma_CS",] * (pars_S["j_ST0",] + (1-pars_S["y_C",])*j_SG[t-1.]/pars_S["y_C",]) #New metabolic CO2 from biomass turnover
    j_CP[t,] <- v.synth(substrateOne = (j_L[t,] * pars_S["y_CL",]), substrateTwo = ((j_CO2[t-1] + r_CH[t-1])*H[t-1]/S.t[t] + r_CS[t,]), max = (pars_S["j_CPm",])) / c_ROS[t-1,] #New photosynthesis rate
    j_eL[t,] <- v.alwaysPositive(rejFlux = (j_L[t,] - j_CP[t,]/pars_S["y_CL",])) #Excess light energy in the system
    j_NPQ[t,] <- (pars_S["k_NPQ",]^(-1)+j_eL[t,]^(-1))^(-1) #New capacity for nonphotochemical quenching
    c_ROS[t,] <- 1 + (v.alwaysPositive(rejFlux = j_eL[t,] - j_NPQ[t,]) / pars_S["k_ROS",])^pars_S["k",] #Creation of ROS relative to baseline levels
    j_SG[t,] <- v.synth(substrateOne = (pars_S["y_C",]*j_CP[t,]), substrateTwo = ((rho_N[t-1]*H[t-1]/S.t[t] + r_NS[t,])/pars_S["n_NS",]), max = (pars_S["j_SGm",])) #Biomass production of Symbiont
    rho_C[t,] <- v.alwaysPositive(rejFlux = (j_CP[t,] - j_SG[t,]/pars_S["y_C",])) #Symbiont produced carbon shared with Host
    j_ST[t,] <- pars_S["j_ST0",] * (1 + pars_S["b",] * (c_ROS[t,] - 1)) #Symbiont biomass turnover rate
    
    dS.Sdt[t,] <- j_SG[t,] - j_ST[t,] #Specific Instantaneous Symbiont growth rate
    dS.dt[t,] <- dS.Sdt[t,] * S[t-1,] #Instantaneous Symbiont growth rate
    S[t,] <- S[t-1,] + dS.dt[t,] * dt #Symbiont biomass at time t
    
    rho_C.t <- sum(rho_C[t,]*S[t-1,]) #Fixed carbon shared with host
    
    #Host
    #----
    j_HG[t] <- synth(substrateOne = (pars_HX$y_C * (rho_C.t/H[t-1] + j_X[t])), substrateTwo = ((j_N[t] + pars_HX$n_NX * j_X[t] + r_NH[t]) / pars_HX$n_NH), max = (pars_HX$j_HGm)) #Host biomass production rate
    rho_N[t] <- alwaysPositive(j_N[t] + pars_HX$n_NX * j_X[t] + r_NH[t] - pars_HX$n_NH * j_HG[t]) #Nitrogen shared with Symbiont
    j_eC[t] <- alwaysPositive(j_X[t] + rho_C.t/H[t-1] - j_HG[t] / pars_HX$y_C) #Excess carbon in the system
    r_CH[t] <- pars_HX$sigma_CH * (pars_HX$j_HT0 + (1-pars_HX$y_C) * j_HG[t] / pars_HX$y_C) #Recycled CO2 from host
    j_CO2[t] <- pars_HX$k_CO2 * j_eC[t] #CO2 sent from host CCMs to symbiont
    
    dH.Hdt[t] <- j_HG[t] - pars_HX$j_HT0 #Specific Instantaneous Host growth rate
    dH.dt[t] <- dH.Hdt[t] * H[t-1] #Host Instantaneous growth rate
    H[t] <- H[t-1] + dH.dt[t] * dt #Host biomass at time t
    
    HS[t] <- H[t]/S.t[t] #Host:Symbiont biomass ratio
    SH[t] <- S.t[t]/H[t] #Symbiont:Host biomass ratio
  }
  
  #Generate and Return output
  return(list(L=env$L, N=env$N, X=env$X, j_X=j_X, j_N=j_N, r_NH=r_NH, rho_N=rho_N, j_eC=j_eC, j_CO2=j_CO2, 
       j_HG=j_HG, r_CH=r_CH, dH.Hdt=dH.Hdt, dH.dt=dH.dt, H=H, r_NS=r_NS, j_L=j_L, 
       j_CP=j_CP, j_eL=j_eL, j_NPQ=j_NPQ, j_SG=j_SG, rho_C=rho_C, j_ST=j_ST, 
       r_CS=r_CS, c_ROS=c_ROS, dS.Sdt=dS.Sdt, dS.dt=dS.dt, S=S, HS=HS, SH=SH, S.t=S.t))
}

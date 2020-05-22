#Function synth returns the production rate of biomass given input rates of 
#two substrates substrateOne and substrateTwo as well as a maximum flux
synth <- function(substrateOne, substrateTwo, max) {
  if (is.na(substrateOne)) {
    return(1)
  }
  if (is.na(substrateTwo)) {
    return(2)
}
  if (substrateOne*substrateTwo*max==0) {
    return(0)
  } else {
  synth<-(1/(max^(-1)+substrateOne^(-1)+substrateTwo^(-1)-(substrateOne+substrateTwo)^(-1)))
  return(synth)
  }
}

#Function MMk is a generic Michaelis-Menten kinetics model that returns the
#assimilation rate of the given substrate
MMk <- function(substrate, max, halfSat) {
  (max*substrate)/(substrate+halfSat)
}

#Function alwaysPositive returns 0 if the given rejection flux rejFlux is
#negative and rejflux if it is greater than zero because rejection fluxes 
#must always be positive
alwaysPositive <- function(rejFlux) {
  max(0,rejFlux)
}

#Function environment returns the initial environment given input lists time,
#L, N, and X that each specify the behavior of the aspects of the environment 
#over time
environment <- function(time, L, N, X) {
  const1=as.numeric(0.0172)
  
  # Light
  outL <- if (L[3]==0) {
    seq(L[1], L[2], along=time)
  } else if (L[3]==1) {
    seq(L[2], L[1], along=time)
  } else {
    0.5 * (L[2] - L[1]) * sin(const1*time) + L[1] + 0.5 * (L[2] - L[1])
  }
  
  # DIN
  outN <- if (N[3]==0) {
    seq(N[1], N[2], along=time)
  } else if (N[3]==1) {
    seq(N[2], N[1], along=time)
  } else {
    0.5 * (N[2] - N[1]) * sin(const1*time) + N[1] + 0.5 * (N[2] - N[1])
  }
  
  # Prey
  outX <- if (X[3]==0) {
    seq(X[1], X[2], along=time)
  } else if (X[3]==1) {
    seq(X[2], X[1], along=time)
  } else {
    0.5 * (X[2] - X[1]) * sin(const1*time) + 0.5 * (X[2] - X[1])
  }
  # Set environment specifications
  env <- list(L=outL, N=outN, X=outX)
  return(env)
}


#Function runCoral models the growth of symbiont and host biomasses over the
#given time interval, and returns a list of vectors that describe the
#state at a given time
runCoral <- function(time, dt, env, pars_HX, pars_S) {
  
  #Create variable lists for Host
  for(var in c("rho_N", "j_eC", "j_CO2", "j_HG",
               "r_CH", "dH.Hdt", "H")){
    assign(var, rep(1, times=length(time)))
  }
  #Create variable lists for Symbiont
  for (var in c("j_L", "j_CP", "j_eL", "j_NPQ", "j_SG", "rho_C", 
              "j_ST", "r_CS", "c_ROS", "dS.Sdt", "S")) {
    assign(var, rep(1, times=length(time)))
  }
  
  #Initial Host fluxes
  j_X <- rep(x = MMk(env$X, pars_HX$j_Xm, pars_HX$K_X), times = length(time)) #Uptake of prey
  j_N <- rep(x = MMk(env$N, pars_HX$j_Nm, pars_HX$K_N), times = length(time)) #Uptake of DIN
  r_NH <- rep(x = (pars_HX$j_HT0 * pars_HX$n_NH * pars_HX$sigma_NH), times = length(time)) #Recycled Nitrogen from host turnover
  rho_N[1] <- j_N[1] #Nitrogen shared with the Symbiont
  j_eC[1] <- pars_HX$j_eC0 #Excess carbon used to activate host CCMs
  j_CO2[1] <- pars_HX$k_CO2 * j_eC[1] #Uptake of CO2
  j_HG <- pars_HX$j_HG0 #Host biomass growth rate
  r_CH[1] <- pars_HX$j_HT0 * pars_HX$sigma_CH #Recycled CO2 from host
  dH.Hdt[1] <- pars_HX$j_HGm #Total host growth rate
  H[1] <- pars_HX$H_0 #Initial host biomass
  
  #Initial Symbiont fluxes
  r_NS <- rep(x = pars_S$j_ST0 * pars_S$n_NS * pars_S$sigma_NS, times = length(time)) #Recycled Nitrogen from Symbiont turnover
  j_L[1] <- env$L[1] * pars_S$aStar #Initial uptake of light
  j_CP[1] <- alwaysPositive(synth(j_L[1] * pars_S$y_CL, j_CO2[1]*H[1]/pars_S$S_0, pars_S$j_CPm)) #Initial production of biomass from photosynthesis
  j_eL[1] <- alwaysPositive(rejFlux=j_L[1] - j_CP[1]/pars_S$y_CL) #Excess light energy in the system
  j_NPQ[1] <- pars_S$k_NPQ #Total capacity of nonphotochemical quenching
  j_SG[1] <- pars_S$j_SGm/10 #Production of Symbiont biomass
  rho_C[1] <- j_CP[1] #Fixed carbon shared with host
  j_ST[1] <- pars_S$j_ST0 #Initial biomass turnover rate
  r_CS[1] <- pars_S$j_ST0 * pars_S$sigma_CS #Recycled CO2 from symbiont
  c_ROS[1] <- 0 #Reactive oxygen species production relative to baseline
  dS.Sdt[1] <- pars_S$j_SGm #Total Symbiont growth rate
  S[1] <- pars_S$S_0 #Initial Symbiont biomass
  
  
  #Run Model
  
  for (t in (2:length(time))) {
    #S.t <- sum(S[t-1,])  For use when having more than one symbiont. For now, use:
    S.t <- S[t-1] #Previous step symbiont biomass
    j_L[t] <- (1.256307 + 1.385969 * exp(-6.479055 * S.t/H[t-1])) * env$L[t] * pars_S$aStar #New uptake of light into symbiont
    r_CS[t] <- pars_S$sigma_CS * (pars_S$j_ST0 + (1-pars_S$y_C)*j_SG[t-1]/pars_S$y_C) #New metabolic CO2 from biomass turnover
    j_CP[t] <- synth(substrateOne = (j_L[t] * pars_S$y_CL), substrateTwo = ((j_CO2[t-1] + r_CH[t-1])*H[t-1]/S.t + r_CS[t]), max = (pars_S$j_CPm) / c_ROS[t-1]) #New photosynthesis rate
    j_eL[t] <- alwaysPositive(rejFlux = (j_L[t] - j_CP[t]/pars_S$y_CL)) #Excess light energy in the system
    j_NPQ[t] <- (pars_S$k_NPQ^(-1)+j_eL[t]^(-1))^(-1/1) #New capacity for nonphotochemical quenching
    c_ROS[t] <- 1 + (alwaysPositive(rejFlux = j_eL[t] - j_NPQ[t]) / pars_S$k_ROS)^pars_S$k #Creation of ROS relative to baseline levels
    j_SG[t] <- synth(substrateOne = (pars_S$y_C*j_CP[t]), substrateTwo = ((rho_N[t-1]*H[t-1]/S.t + r_NS[t-1])/pars_S$n_NS), max = (pars_S$j_SGm)) #Biomass production of Symbiont
    rho_C[t] <- alwaysPositive(rejFlux = (j_CP[t] - j_SG[t]/pars_S$y_C)) #Symbiont produced carbon shared with Host
    j_ST[t] <- pars_S$j_ST0 * (1 + pars_S$b * (c_ROS[t] - 1))
    dS.Sdt[t] <- j_SG[t] - j_ST[t] #Total growth rate of symbiont
    S[t] <- S[t-1] + dS.Sdt[t] * S[t-1] * dt #Symbiont biomass at time t
    #rhoC.t <- sum(rho_C[t,]*S[t-1,])  For use when having more than one symbiont. For now, use:
    rho_C.t <- rho_C[t]*S[t-1] #Symbiont use of carbon
    
    j_HG[t] <- synth(substrateOne = (pars_S$y_C * (rho_C.t/H[t-1] + j_X[t])), substrateTwo = ((j_N[t] + pars_HX$n_NX*j_X[t] + r_NH[t]) / pars_HX$n_NH), max = (pars_HX$j_HGm)) #Host biomass production rate
    rho_N[t] <- alwaysPositive(rejFlux = (j_N[t] + pars_HX$n_NX * j_X[t] + r_NH[t] - pars_HX$nNH * j_HG[t])) #Nitrogen shared with Symbiont
    j_eC[t] <- alwaysPositive(rejFlux = (j_X[t] + rho_C.t/H[t-1] - j_HG[t]/pars_S$y_C)) #Excess carbon in the system
    r_CH[t] <- pars_HX$sigma_CH * (pars_HX$j_HT0 + (1-pars_S$y_C)*j_HG[t]/pars_S$y_C) #Recycled CO2 from host
    j_CO2[t] <- pars_HX$k_CO2 * j_eC[t] #CO2 sent from host CCMs to symbiont
    dH.Hdt[t] <- j_HG[t] - pars_HX$j_HT0 #Total host growth rate
    H[t] <- H[t-1] + dH.Hdt[t] * H[t-1] * dt #Host biomass at time t
  }
  
  #Generate and Return output
  return(list(time, j_X=j_X, j_N=j_N, r_NH=r_NH, rho_N=rho_N, j_eC=j_eC, j_CO2=j_CO2, 
       j_HG=j_HG, r_CH=r_CH, dH.Hdt=dH.Hdt, H=H, r_NS=r_NS, j_L=j_L, 
       j_CP=j_CP, j_eL=j_eL, j_NPQ=j_NPQ, j_SG=j_SG, rho_C=rho_C, j_ST=j_ST, 
       r_CS=r_CS, c_ROS=c_ROS, dS.Sdt=dS.Sdt, S=S))
}

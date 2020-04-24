#Function synth returns the production rate of biomass given input rates of 
#two substrates substrateOne and substrateTwo as well as a maximum flux
synth <- function(substrateOne, substrateTwo, max) {
  1/(max^-1+substrateOne^-1+substrateTwo^-1-(substrateOne+substrateTwo)^-1)
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

#Function environment returns the list of environment vectors along time
#including the initial environment
environment <- function(modelLength, light, nitrogen, prey) {
  list(rep(c(light, nitrogen, prey), modelLength+1))
}

#Function coralStep takes an input state vector for the model and returns the
#next step along time
coralStep <- function(inState=c(time, L, N, X, j_X, j_N, r_NH, rho_N, j_eC,
                                j_CO2, j_HG, r_CH, dH.Hdt, H, r_NS, j_L,
                                j_CP, j_eL, r_NPQ, j_SG, rho_C, j_ST, r_CS,
                                c_ROS, dS.Sdt, S)){
  outState
}

#Function runCoral models the growth of symbiont and host biomasses over the
#given time interval, and returns a list of vectors that describe the
#state at a given time
runCoral <- function(modelLength, env, pars) {
  #Create list of vectors
  data <- list(rep(c(time=NA, L=NA, N=NA, X=NA, j_X=NA,j_N=NA, r_NH=NA, 
                     rho_N=NA, j_eC=NA, j_CO2=NA, j_HG=NA, r_CH=NA, 
                     dH.Hdt=NA, H=NA, r_NS=NA, j_L=NA, j_CP=NA, j_eL=NA, 
                     j_NPQ=NA, j_SG=NA, rho_C=NA, j_ST=NA, r_CS=NA, c_ROS=NA, 
                     dS.Sdt=NA, S=NA), modelLength+1))
}
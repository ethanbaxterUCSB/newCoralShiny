#Default Parameters

#Host and Prey Parameters
defPars_HX <- list(
  #Host
  j_HT0=0.03, #Host biomass turnover rate per day (1/d)
  n_NH=0.18, #N:C (Nitrogen to Carbon) ratio in host biomass (no units)
  sigma_NH=0.9, #Proportion of host nitrogen turnover recycled (no units)
  sigma_CH=0/1, #Proportion of host carbon turnover recycled (no units)
  j_Nm=0.035, #Maximum specific host DIN uptake rate (molN/CmolH/d)
  j_HGm=1, #Maximum specific host growth rate (CmolH/CmolH/d)
  k_CO2=10, #Rate of host CCM's (molCO2/molC/d)
  K_N=1.5e-6, #Half-saturation constant for host DIN uptake (molN/L)
  H_0=1, #Initial host biomass (CmolH)
  
  
  #Prey
  n_NX=0.2, #N:C ratio in prey biomass (no units)
  j_Xm=0.13, #Maximum specific host feeding rate (molX/CmolH/d)
  K_X=1e-6, #Half-saturation constant for host feeding (CmolX/L)
  
)

#Symbiont Parameters
defPars_S <- list(
  
)

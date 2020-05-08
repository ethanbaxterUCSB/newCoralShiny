#Default Parameters
#j's generally denote mass flux
#n's generally denote ratios
#sigma's denote recycled biomass

#Default time inputs
defPars_t <- list(
  length = 365, #Length of the model
  dt = 0.01 #Length of a single time step at which to calculate a state
)

#Default initial environment inputs
defPars_env <- list(
  L=1, #Initial amount of available light
  N=1, #Initial amount of available DIN
  X=1 #Initial amount of available prey
)

#Host and Prey Parameters
defPars_HX <- list(
  #Host
  j_HT0=0.03, #Maintenance rate of host biomass (1/d)
  n_NH=0.18, #N:C (Nitrogen to Carbon) molar ratio in host biomass (no units)
  sigma_NH=0.9, #Proportion of nitrogen turnover recycled in host (no units)
  sigma_CH=0/1, #Proportion of host metabolic CO2 turnover recycled to photosynthesis (no units)
  j_Nm=0.035, #Maximum host DIN (Dissolved Inorganic Nitrogen) uptake rate (molN/CmolH/d)
  j_HGm=1, #Maximum specific host growth rate (CmolH/CmolH/d)
  k_CO2=10, #Efficacy of CO2 delivery to photosynthesis by host CCMs (molCO2/molC/d)
  K_N=1.5e-6, #Half-saturation constant for host DIN uptake (molN/L)
  H_0=1, #Initial host biomass (CmolH)
  j_eC0=10, #Initial excess carbon used to activate host CCMs (molC/CmolH/d)
  j_HG0=0.25, #Initial host biomass formation rate (CmolH/CmolH/d)
  
  
  #Prey
  n_NX=0.2, #N:C ratio in prey biomass (no units)
  j_Xm=0.13, #Maximum specific host feeding rate (molX/CmolH/d)
  K_X=1e-6 #Half-saturation constant for host feeding (CmolX/L)
)

#Symbiont Parameters
defPars_S <- list(
  y_C=0.8, #Yield of biomass formation from carbon (C-mol mol C^-1)
  j_ST0=0.03, #Maintenance rate of symbiont biomass (C-mol S C-mol S^-1 d^-1)
  n_NS=0.13, #N:C ratio in symbiont biomass (no units)
  y_CL=0.1, #L:C (Light to Carbon) ratio in fixed carbon, i.e. quantum yield (molC/mol ph)
  k_NPQ=112, #Capacity of non-photochemical quenching (mol photons/CmolS/d)
  k_ROS=80, #Amount of excess light beyond NPQ capacity that doubles ROS (Reactive Oxygen Species) production relative to baseline (mol ph/CmolS/d)
  k=1, #Exponent on ROS production (no units)
  aStar=1.34, #Effective symbiont specific cross-sectional area for photosynthesis (m^2/C-molS)
  sigma_NS=0.9, #Proportion of nitrogen turnover recycled in symbiont(no units)
  sigma_CS=0.9, #Proportion of metabolic CO2 recycled to photosynthesis (no units)
  j_CPm=2.8, #Maximum specific photosynthesis rate (Cmol/CmolS/d)
  j_SGm=0.25, #Maximum specific symbiont growth rate (CmolS/CmolS/d)
  S_0=1, #Initial symbiont biomass (CmolS)
  b=5 #Scaling parameter for bleaching response
)


#------------------------------------------------------
#Verification of runCoral and runCoral with master runs
#------------------------------------------------------

#My scripts
source("runCoral.R")
source("runCoralSS.R")

#Master scripts
source("master/run_coral.R")
source("master/run_coral_ss.R")

run <- {
  #My scripts
  source("runCoral.R")
  source("defPars.R")
  time <- seq(0, 365, 0.1)
  env <- environment(time, c(30, 30, 0), c(1e-7,1e-7,0), c(1e-7,1e-7,0))
  pars_HX <- defPars_HX
  pars_S <- defPars_S(1)
  pars_S["S_0",] <- 0.00005
  
  myCoral <- as.data.frame(runCoral(time, env, pars_HX, pars_S))
  
  #Master scripts
  source("master/run_coral.R")
  source("master/def_pars.R")
  source("master/synth.R")
  pars <- def_pars(1)
  pars["initS"] <- 0.00005
  
  master <- run_coral(time, env, pars)
  
  plot(time, myCoral$dH.Hdt - master$dH.Hdt, "l")
  plot(time[3:3651], myCoral$dH.Hdt[3:3651], col = "2", "l")
  plot(time[3:3651], master$dH.Hdt[3:3651], col = "3", "l")
}

ss <- {
  #My scripts
  source("runCoralSS.R")
  source("defPars.R")
  
  pars_HX <- defPars_HX
  pars_S <- defPars_S(1)
  env <- c(L = 30, N = 1e-7, X = 1e-7)
  
  mySS <- runCoralSS(env, pars_HX, pars_S)
  
  #Master scripts
  source("master/run_coral_ss.R")
  source("master/synth.R")
  source("master/def_pars.R")
  
  pars <- def_pars(1)
  env <- list(L = 30, N = 1e-7, X = 1e-7)
  masterSS <- run_coral_ss(env, pars, dt = 0.1)
  
  unlist(mySS$dH.Hdt)[length(unlist(mySS$dH.Hdt))] - masterSS$H$dH.Hdt[length(masterSS$H$dH.Hdt)]
}

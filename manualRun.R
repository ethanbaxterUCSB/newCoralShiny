source("runCoral.R")
source("defPars.R")
source("coralPlots.R")
source("runCoralSS.R")
run <- {
  time <- seq(0,365,0.1)
  L <- c(20,40,0)
  N <- c(1e-6,1e-6,0)
  X <- c(1e-7,1e-7,0)
  env <- c(L = 30, N = 1e-7, X = 1e-7)
  coral <- runCoralSS(env, defPars_HX, defPars_S(1))
  
  coralPlots.SH(time = coral$time, SH = coral$SH)
  
}

runSS <- {
  pars_HX <- defPars_HX
  pars_S <- defPars_S(1)
  env <- c(L = 30, N = 1e-7, X = 1e-6)
  coral <- runCoralSS(env = env, pars_HX = pars_HX, pars_S = pars_S)
  plot(1:length(unlist(coral$S)), unlist(coral$SH), "l")
}

as.data.frame(runCoral(time, env, defPars_HX, defPars_S(3)))


run <- {
  source("runCoral.R")
  source("defPars.R")
  source("coralPlots.R")
  time <- seq(0,365,0.1)
  L <- c(20,40,0)
  N <- c(1e-6,1e-6,0)
  X <- c(1e-7,1e-7,0)
  env <- environment(time, L, N, X)
  coral <- runCoral(time, env, defPars_HX, defPars_S(3))
  
  coralPlots.S.t(time = time, S = coral$S.t)
  
}

as.data.frame(runCoral(time, env, defPars_HX, defPars_S(3)))

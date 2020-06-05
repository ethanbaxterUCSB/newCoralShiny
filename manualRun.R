
run <- {
  source("runCoral.R")
  source("defPars.R")
  time <- seq(0,365,15)
  L <- c(1,1,0)
  N <- c(1e-6,1e-6,0)
  X <- c(0,0,0)
  env <- environment(time, L, N, X)
  run <- runCoral(time, env, defPars_HX, defPars_S)
  
  plot(x=time, run$H, "l", col="black")
  lines(x=time, run$S, "l", col="red")
  lines(x=time, run$H/run$S, "l", col="blue")
}

as.data.frame(runCoral(time, env, defPars_HX, defPars_S))

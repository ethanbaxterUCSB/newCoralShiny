
run <- {
  source("runCoral.R")
  source("defPars.R")
  time <- seq(0,365,0.1)
  L <- c(20,40,0)
  N <- c(1e-6,1e-6,0)
  X <- c(1e-7,1e-7,0)
  env <- environment(time, L, N, X)
  run <- runCoral(time, env, defPars_HX, defPars_S)
  
  plot(x=time, run$H, "l", col="black")
  lines(x=time, run$S, "l", col="red")
  lines(x=time, run$HS, "l", col="blue")
  
  plot(x=time[3:length(time)], run$dH.Hdt[3:length(time)], "l")
  lines(x=time[3:length(time)], run$dS.Sdt[3:length(time)], "l")
}

as.data.frame(runCoral(time, env, defPars_HX, defPars_S))


#------------------------------------------------------
#Verification of runCoral and runCoralSS with master runs
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
  env <- c(L = 20, N = 1e-8, X = 1e-7)
  
  mySS <- runCoralSS(env, pars_HX, pars_S)
  
  #Master scripts
  source("master/run_coral_ss.R")
  source("master/synth.R")
  source("master/def_pars.R")
  
  pars <- def_pars(1)
  env <- as.list(env)
  masterSS <- run_coral_ss(env, pars, dt = 0.1)
  
  print(unlist(mySS$dH.Hdt)[length(unlist(mySS$dH.Hdt))] - masterSS$H$dH.Hdt[length(masterSS$H$dH.Hdt)])
}

runOverVariedEnv <- {
  source("runCoral.R")
  source("defPars.R")
  source("master/run_coral.R")
  source("master/def_pars.R")
  #Normal runs with constant env
  x <- list(range = c(20,30), res = 1, name = "L")
  y <- list(range = c(0, 1e-6), res = 1e-7, name = "X")
  sens <- 0.001
  
  time <- seq(from = 0, to = 365, by = 0.1)
  env <- c(L = 30, N = 1e-7, X = 1e-7)
  myPars <- list(HX = defPars_HX, S = defPars_S(1))
  masterPars <- def_pars(1)
  
  xAxis <- seq(from = x$range[1], to = x$range[2], by = x$res)
  yAxis <- seq(from = y$range[1], to = y$range[2], by = y$res)
  imageData <- matrix(nrow = length(xAxis), ncol = length(yAxis))
  for (i in 1:length(xAxis)) {
    print(paste("X", i))
    env[x$name] <- xAxis[i]
    for (j in 1:length(yAxis)) {
      print(paste("Y", j))
      env[y$name] <- yAxis[j]
      newEnv <- environment(time, c(env["L"], env["L"], 0), c(env["N"], env["N"], 0), c(env["X"], env["X"], 0))
      myCoral <- runCoral(time, newEnv, myPars$HX, myPars$S)
      masterCoral <- run_coral(time, newEnv, masterPars)
      imageData[i,j] <- abs(mean(myCoral$SH - masterCoral$S/masterCoral$H))
    }
  }
  #Data map
  image(xAxis, yAxis, imageData, zlim = c(0, sens), xlab = "L from 20 to 30 by 1", ylab = "X from 0 to 1e-6 by 1e-7", main = "Abs. difference")
  #Scale
  image(as.matrix(seq(from = 0, to = sens, by = sens/10)))
  title(xlab = "L from 20 to 30 by 1", ylab = "X from 0 to 1e-6 by 1e-7", main = "Absolute of value of difference between runCoral.R and run_Coral.R on scale of 0 to 0.001")
}

#model for newton's law of cooling
DE <- function(h=h, T_e=T_e, T_o=T_o) {-1*h*(T_o-T_e)}

#vars
time <- c(1:input$time)
h <- input$h #proportionality constant
T_e <- input$T_e #environment temp
T_o <- input$T_o #initial temp

temps <- data.frame(time, h, T_e, T_o)

for(t in frame$time[3]:length(frame$time))
  frame$T_o[t]<-frame$T_o[t-1] + DE(frame$h[t-1], frame$T_e[t-1], frame$T_o[t-1])

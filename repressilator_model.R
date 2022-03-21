library(deSolve)
library(reshape2)
repressilator_model <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dm1 <-  -m1 + a/(1 + p3^2) + a0
    dp1 <-  -b * (p1 - m1)
    dm2 <-  -m2 + a/(1 + p1^2) + a0
    dp2 <-  -b * (p2 - m2)
    dm3 <-  -m3 + a/(1 + p2^2) + a0
    dp3 <-  -b * (p3 - m3)
    list(c(dm1, dp1,dm2,dp2,dm3,dp3))
  })
}

parameters <- c(a0 = 0.0005, a = 0.5, b = 5)
state      <- c(m1 = 10, m2 = 10, m3 = 10, p1 = 0, p2 = 0, p3 = 0)
times      <- seq(0, 100, by = 1)

out <- ode(y = state, times = times, 
           func = repressilator_model, parms = parameters)

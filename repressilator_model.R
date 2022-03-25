library(deSolve)
library(reshape2)
repressilator_model <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dA <-  -A + a/(1 + Z^n) + a0
    dX <-  -b * (X - A)
    dB <-  -B + a/(1 + X^n) + a0
    dY <-  -b * (Y - B)
    dC <-  -C + a/(1 + Y^n) + a0
    dZ <-  -b * (Z - C)
    list(c(dA, dB, dC, dX, dY, dZ))
  })
}

parameters <- c(a0 = 0.5, a = 500, b = 5, n = 2)
state      <- c(A = 10, B = 9, C = 8, X = 0, Y = 0, Z = 0) 
#in this model the initial of A, B and C, which reffers to mRNA, should not set as the same 
#if a synthetic oscilatory is expected.
times      <- seq(0, 100, by = 0.1)

out <- ode(y = state, times = times, 
           func = repressilator_model, parms = parameters)

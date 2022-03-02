library(deSolve)
library(reshape2)
LV_model <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dX <-  b * X - p * X * Y
    dY <-  r * X * Y - d * Y
    list(c(dX, dY))
  })
}

parameters <- c(b = 1, p = 0.1, r = 0.02, d = 0.5)
state      <- c(X = 15, Y = 15)
times      <- seq(0, 50, by = 0.05)

out <- ode(y = state, times = times, 
           func = LV_model, parms = parameters)
out <- data.frame(out)
out2 <- melt(out, id.vars = 'time')

col=c("#99CCCC", "#CCFF99")

ggplot(data = out2, aes(time, value, group = variable, 
                        color = variable)) +
  geom_line() +
  labs(x="time",y="population")

ggplot(data = out, aes(X, Y))+
  geom_path(color="purple")+
  labs(x = "X population", y = "Y population")

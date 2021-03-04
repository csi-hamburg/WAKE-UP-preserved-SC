## Inverse Probability Weighting to estimate multiple mediation models (VanderWeele 2015, DOI:10.1515/em-2012-0010)

IPW.fun <- function (df, index) {
  
  data.new <- df[index, ]
  
  Call.num$data <- data.new
  Call.denom$data <- data.new
  Call.out$data <- data.new
  
  
  new.fit.num <- NULL
  new.fit.denom <- NULL
  new.fit.out <- NULL
  
  new.fit.num <- fit_speedglm(Call.num)
  new.fit.denom <- fit_speedglm(Call.denom)
  new.fit.out <- fit_speedglm(Call.out)
  
  
  # E[Y_0M0]
  w0 <- (1 - predict(new.fit.num, newdata = data.new, type = 'response')) / (1 - predict(new.fit.denom, newdata = data.new, type = 'response'))
  E.Y_0M0 <- mean(data.new$y[data.new$x == 'c'] * w0[data.new$x == 'c'])
  
  
  # E[Y_1M1]
  w1 <- predict(new.fit.num, newdata = data.new, type = 'response') / predict(new.fit.denom, newdata = data.new, type = 'response')
  E.Y_1M1 <- mean(data.new$y[data.new$x == 't'] * w1[data.new$x == 't'])
  
  # E[Y_1M0]
  df.t <- data.new
  df.t$x <- factor('t', levels = levels(data.new$x))
  p <- predict(new.fit.out, newdata = df.t, type = 'response') 
  E.Y_1M0 <- mean(p[data.new$x == 'c'] * w0[data.new$x == 'c'])
  
  # NDE
  NDE <- (E.Y_1M0 / (1 - E.Y_1M0) ) / ( E.Y_0M0 / (1 - E.Y_0M0) )
  # NIE
  NIE <- (E.Y_1M1 / (1 - E.Y_1M1) ) / ( E.Y_1M0 / (1 - E.Y_1M0) )
  
  
  c(NDE = NDE
    , NIE = NIE
    , TE = NDE*NIE
    , propOR = log(NIE) / (log(NDE) + log(NIE))
    , propLin = (E.Y_1M1 - E.Y_1M0) / (E.Y_1M1 - E.Y_0M0)
  )
  
}


fit_speedglm <- function(x) {
  speedglm::speedglm(formula = x$formula,
                     data = x$data,
                     family = eval(x$family),
                     weights = x$weights)
}


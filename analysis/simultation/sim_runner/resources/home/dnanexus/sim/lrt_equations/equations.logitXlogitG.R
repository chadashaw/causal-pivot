source('../lrt_equations/equations.logitG.R')
source('../lrt_equations/equations.logitX.R')

define.logitXlogitG.equations <- function() {
  
  s <- 0.9
  
  RXY1 <- function(alpha, beta, X)  {
    1/((1 + raise.power(exp(1),-alpha - X*beta))*(1/(1 + raise.power(exp(1),-alpha)) + 0.5*((2*raise.power(beta,2))/(raise.power(exp(1),2*alpha)*raise.power(1 + raise.power(exp(1),-alpha),3)) - 
                                                                                              raise.power(beta,2)/(raise.power(exp(1),alpha)*raise.power(1 + raise.power(exp(1),-alpha),2)))))
  }
  
  RXY0 <- function(alpha, beta, X)  {
    (1 - 1/(1 + raise.power(exp(1),-alpha - X*beta)))/
      (1 - 1/(1 + raise.power(exp(1),-alpha - X*beta)) + 0.5*((-2*raise.power(beta,2))/(raise.power(exp(1),2*alpha)*raise.power(1 + raise.power(exp(1),-alpha),3)) + raise.power(beta,2)/(raise.power(exp(1),alpha)*raise.power(1 + raise.power(exp(1),-alpha),2))))
  }
  
  
  rXY1 <- function(alpha, beta, X) {
    log(RXY1(alpha, beta, X))
  }
  
  rXY0 <- function(alpha, beta, X) {
    log(RXY0(alpha, beta, X))
  }
  
  L <- function(params, omega, alpha, beta, X, Y, G, cases.only) {
    s * lrt.logitG.equations$L(params, omega, alpha, beta, X, Y, G, cases.only) +
      (1 - s) * lrt.logitX.equations$L(params, alpha, beta, X, Y, G, cases.only)
  }
  
  D <- function(params, omega, alpha, beta, X, Y, G, cases.only) {
    DG <- lrt.logitG.equations$D(params, omega, alpha, beta, X, Y, G, cases.only)
    DX <- lrt.logitX.equations$D(params, alpha, beta, X, Y, G, cases.only)
    
    s * DG + (1 - s) * DX
  }
  
  LR <- function(omega, alpha, beta, gamma, eta, X, Y, G, cases.only) {
    alt.sum <- L(c(gamma, eta), omega, alpha, beta, X, Y, G, cases.only)
    null.sum <- (1 - s) * (
      sum(rXY1(alpha, beta, X[which(Y == 1)])) + sum(ifelse(cases.only, 0, rXY0(alpha, beta, X[which(Y == 0)])))
    )
    
    alt.sum - null.sum
  }
  
  list(
    L = L,
    LR = LR,
    D = D
  )
}

lrt.logitXlogitG.equations <- define.logitXlogitG.equations()
rm(define.logitXlogitG.equations)

# list2env(lrt.logitXlogitG.equations, envir = environment())
# 
# params <- list(
#   n = 5e5,
#   omega = 0.001,
#   alpha = -2.2,
#   beta = 0.6,
#   gamma = 0.3,
#   eta = -0.4
# )
# 
# sim.result <- f.call(sim, params)
# 
# run.mle(sim.result, params, cases.only = T)


define.logitX.equations <- function() {
  
  R1 <- function(alpha, beta, gamma, eta, X) {
    1/((1 + raise.power(exp(1),-alpha - X*beta - gamma - X*eta))*(1/(1 + raise.power(exp(1),-alpha - gamma)) + 
                                                                    0.5*((2*raise.power(exp(1),-2*alpha - 2*gamma)*raise.power(-beta - eta,2))/raise.power(1 + raise.power(exp(1),-alpha - gamma),3) - (raise.power(exp(1),-alpha - gamma)*raise.power(-beta - eta,2))/raise.power(1 + raise.power(exp(1),-alpha - gamma),2))))
    
  }
  
  R2 <- function(alpha, beta, gamma, eta, X) {
    1/((1 + raise.power(exp(1),-alpha - X*beta))*(1/(1 + raise.power(exp(1),-alpha)) + 0.5*((2*raise.power(beta,2))/(raise.power(exp(1),2*alpha)*raise.power(1 + raise.power(exp(1),-alpha),3)) - 
                                                                                              raise.power(beta,2)/(raise.power(exp(1),alpha)*raise.power(1 + raise.power(exp(1),-alpha),2)))))
    
  }
  
  R3 <- function(alpha, beta, gamma, eta, X) {
  }
  
  R4 <- function(alpha, beta, gamma, eta, X) {
  }
  
  RXY1 <- function(alpha, beta, X)  {
    1/((1 + raise.power(exp(1),-alpha - X*beta))*(1/(1 + raise.power(exp(1),-alpha)) + 0.5*((2*raise.power(beta,2))/(raise.power(exp(1),2*alpha)*raise.power(1 + raise.power(exp(1),-alpha),3)) - 
                                                                                              raise.power(beta,2)/(raise.power(exp(1),alpha)*raise.power(1 + raise.power(exp(1),-alpha),2)))))
  }
  
  RXY0 <- function(alpha, beta, X)  {
    (1 - 1/(1 + raise.power(exp(1),-alpha - X*beta)))/
      (1 - 1/(1 + raise.power(exp(1),-alpha - X*beta)) + 0.5*((-2*raise.power(beta,2))/(raise.power(exp(1),2*alpha)*raise.power(1 + raise.power(exp(1),-alpha),3)) + raise.power(beta,2)/(raise.power(exp(1),alpha)*raise.power(1 + raise.power(exp(1),-alpha),2))))
  }
  
  lR1 <- function(alpha, beta, gamma, eta, X) {
    log(R1(alpha, beta, gamma, eta, X))
  }
  
  lR2 <- function(alpha, beta, gamma, eta, X) {
    log(R2(alpha, beta, gamma, eta, X))
  }
  
  lR3 <- function(alpha, beta, gamma, eta, X) {
    log(R3(alpha, beta, gamma, eta, X))
  }
  
  lR4 <- function(alpha, beta, gamma, eta, X) {
    log(R4(alpha, beta, gamma, eta, X))
  }
  
  rXY1 <- function(alpha, beta, X) {
    log(RXY1(alpha, beta, X))
  }
  
  rXY0 <- function(alpha, beta, X) {
    log(RXY0(alpha, beta, X))
  }
  
  DR1Gamma <- function(alpha, beta, gamma, eta, X) {
    (-0.5*raise.power(exp(1),alpha + gamma)*(-2. - 12.*raise.power(exp(1),2*(alpha + gamma)) - 2.*raise.power(exp(1),4*(alpha + gamma)) - 4.*raise.power(beta,2) - 8.*beta*eta - 4.*raise.power(eta,2) + 
                                               raise.power(exp(1),alpha + gamma)*(-8. - 6.*raise.power(beta,2) - 12.*beta*eta - 6.*raise.power(eta,2)) + raise.power(exp(1),2*alpha + 2*gamma + X*(beta + eta))*(12. - 6.*raise.power(beta,2) - 12.*beta*eta - 6.*raise.power(eta,2)) + 
                                               raise.power(exp(1),alpha + gamma + X*(beta + eta))*(8. - 2.*raise.power(beta,2) - 4.*beta*eta - 2.*raise.power(eta,2)) + 
                                               raise.power(exp(1),3*alpha + 3*gamma + X*(beta + eta))*(8. - 2.*raise.power(beta,2) - 4.*beta*eta - 2.*raise.power(eta,2)) + raise.power(exp(1),X*(beta + eta))*(2. + 1.*raise.power(beta,2) + 2.*beta*eta + 1.*raise.power(eta,2)) + 
                                               raise.power(exp(1),4*alpha + 4*gamma + X*(beta + eta))*(2. + 1.*raise.power(beta,2) + 2.*beta*eta + 1.*raise.power(eta,2)) + raise.power(exp(1),3*(alpha + gamma))*(-8. + 2.*raise.power(beta,2) + 4.*beta*eta + 2.*raise.power(eta,2)))) /
      (raise.power(1. + raise.power(exp(1),alpha + gamma),3)*(1. + raise.power(exp(1),alpha + gamma + X*(beta + eta)))*
        (1. + raise.power(exp(1),2*(alpha + gamma)) + 0.5*raise.power(beta,2) + 1.*beta*eta + 0.5*raise.power(eta,2) + raise.power(exp(1),alpha + gamma)*(2. - 0.5*raise.power(beta,2) - 1.*beta*eta - 0.5*raise.power(eta,2))))
    
  }
  
  DR1Eta <- function(alpha, beta, gamma, eta, X) {
    ((-1. + 1.*raise.power(exp(1),alpha + gamma) - 1.*raise.power(exp(1),alpha + gamma + X*(beta + eta)) + 1.*raise.power(exp(1),2*alpha + 2*gamma + X*(beta + eta)))*(beta + eta) + 
       X*(1. + 1.*raise.power(exp(1),2*(alpha + gamma)) + 0.5*raise.power(beta,2) + 1.*beta*eta + 0.5*raise.power(eta,2) + raise.power(exp(1),alpha + gamma)*(2. - 0.5*raise.power(beta,2) - 1.*beta*eta - 0.5*raise.power(eta,2))))/
      ((1. + raise.power(exp(1),alpha + gamma + X*(beta + eta)))*(1. + raise.power(exp(1),2*(alpha + gamma)) + 0.5*raise.power(beta,2) + 1.*beta*eta + 0.5*raise.power(eta,2) + 
                                                                    raise.power(exp(1),alpha + gamma)*(2. - 0.5*raise.power(beta,2) - 1.*beta*eta - 0.5*raise.power(eta,2))))
    
  }
  
  DR2Gamma <- function(alpha, beta, gamma, eta, X) { 0 }
  DR2Eta <- function(alpha, beta, gamma, eta, X) { 0 }
  
  DR3Gamma <- function(alpha, beta, gamma, eta, X) {}
  DR3Eta <- function(alpha, beta, gamma, eta, X) {}
  
  DR4Gamma <- function(alpha, beta, gamma, eta, X) {}
  DR4Eta <- function(alpha, beta, gamma, eta, X) {}
  
  GRPSUM <- function(alpha, beta, gamma, eta, X, Y, G, f1, f2, f3, f4, cases.only = T) {
    grp1 <- which(Y == 1 & G == 1)
    grp2 <- which(Y == 1 & G == 0)
    
    cases.only.sum <- sum(
      sum(f1(alpha, beta, gamma, eta, X[grp1])),
      sum(f2(alpha, beta, gamma, eta, X[grp2]))
    )
    
    if (cases.only) {
      return(cases.only.sum)
    }
    
    grp3 <- which(Y == 0 & G == 1)
    grp4 <- which(Y == 0 & G == 0)
    
    sum(
      cases.only.sum,
      sum(f3(alpha, beta, gamma, eta, X[grp3])),
      sum(f4(alpha, beta, gamma, eta, X[grp4]))
    )
  }
  
  DGamma <- function(alpha, beta, gamma, eta, X, Y, G, cases.only = T) {
    GRPSUM(alpha, beta, gamma, eta, X, Y, G, DR1Gamma, DR2Gamma, DR3Gamma, DR4Gamma, cases.only)
  }
  
  DEta <- function(alpha, beta, gamma, eta, X, Y, G, cases.only = T) {
    GRPSUM(alpha, beta, gamma, eta, X, Y, G, DR1Eta, DR2Eta, DR3Eta, DR4Eta, cases.only)
  }
  
  ll.alt <- function(alpha, beta, gamma, eta, X, Y, G, cases.only) {
    GRPSUM(alpha, beta, gamma, eta, X, Y, G, lR1, lR2, lR3, lR4, cases.only)
  }
  
  ll.null <- function(alpha, beta, X, Y, cases.only) {
    sum(rXY1(alpha, beta, X[which(Y == 1)])) + sum(ifelse(cases.only, 0, rXY0(alpha, beta, X[which(Y == 0)])))
  }
  
  L.ALT <- function(params, alpha, beta, X, Y, G, cases.only) {
    gamma <- params[1]
    eta <- params[2]
    
    ll.alt(alpha, beta, gamma, eta, X, Y, G, cases.only)
  }
  
  LR <- function(alpha, beta, gamma, eta, X, Y, G, cases.only = T) {
    alt.sum <- GRPSUM(alpha, beta, gamma, eta, X, Y, G, lR1, lR2, lR3, lR4, cases.only)
    null.sum <- sum(rXY1(alpha, beta, X[which(Y == 1)])) + sum(ifelse(cases.only, 0, rXY0(alpha, beta, X[which(Y == 0)])))
    
    alt.sum - null.sum
  }
  
  L <- function(params, alpha, beta, X, Y, G, cases.only) {
    gamma <- params[1]
    eta <- params[2]
    
    LR(alpha, beta, gamma, eta, X, Y, G, cases.only)
  }
  
  D <- function(params, alpha, beta, X, Y, G, cases.only) {
    gamma <- params[1]
    eta <- params[2]
    c(
      # cases only cumulative derivative wrt gamma
      DGamma(alpha, beta, gamma, eta, X, Y, G, cases.only),
      # cases only cumulative derivative wrt eta
      DEta(alpha, beta, gamma, eta, X, Y, G, cases.only)
    )
  }
  
  # fisher.info <- function(omega, alpha, beta, gamma, eta, X, Y, G) {
  #   calc.I <- function(omega, alpha, beta, gamma, eta, X, dGamma, dEta) {
  #     f.params <- list(
  #       omega = omega,
  #       alpha = alpha,
  #       beta = beta,
  #       gamma = gamma,
  #       eta = eta,
  #       X = X
  #     )
  #     g <- f.call(dGamma, f.params)
  #     e <- f.call(dEta, f.params)
  #     outer.product <- mapply(function(x, y) outer(c(x,y), c(x,y)), g, e, SIMPLIFY=F)
  #     Reduce("+", outer.product) / length(outer.product)
  #   }
  #   
  #   grp1 <- which(Y == 1 & G == 1)
  #   grp2 <- which(Y == 1 & G == 0)
  #   grp3 <- which(Y == 0 & G == 1)
  #   grp4 <- which(Y == 0 & G == 0)
  #   
  #   I1 <- calc.I(omega, alpha, beta, gamma, eta, X[grp1], DR1Gamma, DR1Eta)
  #   I2 <- calc.I(omega, alpha, beta, gamma, eta, X[grp2], DR2Gamma, DR2Eta)
  #   I3 <- calc.I(omega, alpha, beta, gamma, eta, X[grp3], DR3Gamma, DR3Eta)
  #   I4 <- calc.I(omega, alpha, beta, gamma, eta, X[grp4], DR4Gamma, DR4Eta)
  #   
  #   list(
  #     n1 = length(grp1),
  #     I1 = I1,
  #     n2 = length(grp2),
  #     I2 = I2,
  #     n3 = length(grp3),
  #     I3 = I3,
  #     n4 = length(grp4),
  #     I4 = I4
  #   )
  # }
  
  list(
    L = L.ALT,
    LR = LR,
    D =  D
  )
}

lrt.logitX.equations <- define.logitX.equations()
rm(define.logitX.equations)
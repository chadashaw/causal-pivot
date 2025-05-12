define.equations.logitXG <- function() {
  params.0 <- list(
    n = 5e5,
    omega = 1e-3,
    alpha = -3,
    beta = 0.6
  )
  
  fY1XG1 <- function(alpha, beta, gamma, eta, X) {
    1 / (1 + exp(-1 * (alpha + beta * X + gamma + eta * X)))
    # expit(my.logit(alpha, beta, gamma, eta, X, 1))
  }
  
  fY1XG0 <- function(alpha, beta, X) {
    # expit(my.logit(alpha, beta, 0, 0, X, 0))
    1 / (1 + exp(-1 * (alpha + beta * X)))
  }
  
  fY0XG1 <- function(alpha, beta, gamma, eta, X) {
    1 - fY1XG1(alpha, beta, gamma, eta, X)
  }
  
  fY0XG0 <- function(alpha, beta, X) {
    1 - fY1XG0(alpha, beta, X)
  }
  
  R1 <- function(omega, alpha, beta, gamma, eta, X) {
    1/((1 + raise.power(exp(1),-alpha - X*beta - gamma - X*eta))*((1/(1 + raise.power(exp(1),-alpha)) + 0.5*
                                                                     ((2*raise.power(beta,2))/(raise.power(exp(1),2*alpha)*raise.power(1 + raise.power(exp(1),-alpha),3)) - raise.power(beta,2)/(raise.power(exp(1),alpha)*raise.power(1 + raise.power(exp(1),-alpha),2))))*(1 - omega) + 
                                                                    (1/(1 + raise.power(exp(1),-alpha - gamma)) + 0.5*((2*raise.power(exp(1),-2*alpha - 2*gamma)*raise.power(-beta - eta,2))/raise.power(1 + raise.power(exp(1),-alpha - gamma),3) - 
                                                                                                                         (raise.power(exp(1),-alpha - gamma)*raise.power(-beta - eta,2))/raise.power(1 + raise.power(exp(1),-alpha - gamma),2)))*omega))
  }
  
  R2 <- function(omega, alpha, beta, gamma, eta, X) {
    1/((1 + raise.power(exp(1),-alpha - X*beta))*((1/(1 + raise.power(exp(1),-alpha)) + 0.5*((2*raise.power(beta,2))/(raise.power(exp(1),2*alpha)*raise.power(1 + raise.power(exp(1),-alpha),3)) - 
                                                                                               raise.power(beta,2)/(raise.power(exp(1),alpha)*raise.power(1 + raise.power(exp(1),-alpha),2))))*(1 - omega) + 
                                                    (1/(1 + raise.power(exp(1),-alpha - gamma)) + 0.5*((2*raise.power(exp(1),-2*alpha - 2*gamma)*raise.power(-beta - eta,2))/raise.power(1 + raise.power(exp(1),-alpha - gamma),3) - 
                                                                                                         (raise.power(exp(1),-alpha - gamma)*raise.power(-beta - eta,2))/raise.power(1 + raise.power(exp(1),-alpha - gamma),2)))*omega))
    
  }
  
  R3 <- function(omega, alpha, beta, gamma, eta, X) {

  }
  
  R4 <- function(omega, alpha, beta, gamma, eta, X) {

  }
  
  RXY1 <- function(alpha, beta, X)  {
    1/((1 + raise.power(exp(1),-alpha - X*beta))*(1/(1 + raise.power(exp(1),-alpha)) + 0.5*((2*raise.power(beta,2))/(raise.power(exp(1),2*alpha)*raise.power(1 + raise.power(exp(1),-alpha),3)) - 
                                                                                              raise.power(beta,2)/(raise.power(exp(1),alpha)*raise.power(1 + raise.power(exp(1),-alpha),2)))))
    
  }
  
  RXY0 <- function(alpha, beta, X)  {
    (1 - 1/(1 + raise.power(exp(1),-alpha - X*beta)))/
      (1 - 1/(1 + raise.power(exp(1),-alpha - X*beta)) + 0.5*((-2*raise.power(beta,2))/(raise.power(exp(1),2*alpha)*raise.power(1 + raise.power(exp(1),-alpha),3)) + raise.power(beta,2)/(raise.power(exp(1),alpha)*raise.power(1 + raise.power(exp(1),-alpha),2))))
    
  }
  
  lR1 <- function(omega, alpha, beta, gamma, eta, X) {
    log(R1(omega, alpha, beta, gamma, eta, X))
  }
  
  lR2 <- function(omega, alpha, beta, gamma, eta, X) {
    log(R2(omega, alpha, beta, gamma, eta, X))
  }
  
  lR3 <- function(omega, alpha, beta, gamma, eta, X) {
    log(R3(omega, alpha, beta, gamma, eta, X))
  }
  
  lR4 <- function(omega, alpha, beta, gamma, eta, X) {
    log(R4(omega, alpha, beta, gamma, eta, X))
  }
  
  rXY1 <- function(alpha, beta, X) {
    log(RXY1(alpha, beta, X))
  }
  
  rXY0 <- function(alpha, beta, X) {
    log(RXY0(alpha, beta, X))
  }
  
  DR1Gamma <- function(omega, alpha, beta, gamma, eta, X) {
    (raise.power(exp(1),-6*alpha - 4*gamma - X*(beta + eta))*(-0.5*raise.power(exp(1),2*alpha)*(1. + raise.power(exp(1),alpha + gamma + X*(beta + eta)))*
                                                                (2. + 1.*raise.power(beta,2) + 2.*beta*eta + 1.*raise.power(eta,2) + raise.power(exp(1),alpha + gamma)*(4. - 4.*raise.power(beta,2) - 8.*beta*eta - 4.*raise.power(eta,2)) + 
                                                                   raise.power(exp(1),2*(alpha + gamma))*(2. + 1.*raise.power(beta,2) + 2.*beta*eta + 1.*raise.power(eta,2)))*omega + 
                                                                ((1 + raise.power(exp(1),-alpha - gamma))*(-1.*raise.power(1. + raise.power(exp(1),alpha + gamma),3)*(1. + raise.power(exp(1),2*alpha) + 0.5*raise.power(beta,2) + raise.power(exp(1),alpha)*(2. - 0.5*raise.power(beta,2)))*(-1. + omega) + 
                                                                                                             raise.power(exp(1),gamma)*raise.power(1. + raise.power(exp(1),alpha),3)*(1. + raise.power(exp(1),2*(alpha + gamma)) + 0.5*raise.power(beta,2) + 1.*beta*eta + 0.5*raise.power(eta,2) + 
                                                                                                                                                                                        raise.power(exp(1),alpha + gamma)*(2. - 0.5*raise.power(beta,2) - 1.*beta*eta - 0.5*raise.power(eta,2)))*omega))/raise.power(1 + raise.power(exp(1),-alpha),3)))/
      (raise.power(1 + raise.power(exp(1),-alpha - gamma),4)*(1 + raise.power(exp(1),-alpha - gamma - X*(beta + eta)))*
         ((-1.*raise.power(exp(1),alpha)*(1. + raise.power(exp(1),2*alpha) + 0.5*raise.power(beta,2) + raise.power(exp(1),alpha)*(2. - 0.5*raise.power(beta,2)))*(-1. + omega))/raise.power(1. + raise.power(exp(1),alpha),3) + 
            (raise.power(exp(1),alpha + gamma)*(1. + raise.power(exp(1),2*(alpha + gamma)) + 0.5*raise.power(beta,2) + 1.*beta*eta + 0.5*raise.power(eta,2) + 
                                                  raise.power(exp(1),alpha + gamma)*(2. - 0.5*raise.power(beta,2) - 1.*beta*eta - 0.5*raise.power(eta,2)))*omega)/raise.power(1. + raise.power(exp(1),alpha + gamma),3)))
    
  }
  
  DR1Eta <- function(omega, alpha, beta, gamma, eta, X) {
    (-1.*raise.power(exp(1),-6*alpha - 4*gamma - X*(beta + eta))*(1.*raise.power(1. + raise.power(exp(1),alpha + gamma),3)*X*(1. + raise.power(exp(1),2*alpha) + 0.5*raise.power(beta,2) + raise.power(exp(1),alpha)*(2. - 0.5*raise.power(beta,2)))*
                                                                    (-1. + omega) - 1.*raise.power(exp(1),gamma)*raise.power(1. + raise.power(exp(1),alpha),3)*(-1. + raise.power(exp(1),alpha + gamma))*(1. + raise.power(exp(1),alpha + gamma + X*(beta + eta)))*(beta + eta)*omega - 
                                                                    1.*raise.power(exp(1),gamma)*raise.power(1. + raise.power(exp(1),alpha),3)*X*(1. + raise.power(exp(1),2*(alpha + gamma)) + 0.5*raise.power(beta,2) + 1.*beta*eta + 0.5*raise.power(eta,2) + 
                                                                                                                                                    raise.power(exp(1),alpha + gamma)*(2. - 0.5*raise.power(beta,2) - 1.*beta*eta - 0.5*raise.power(eta,2)))*omega))/
      (raise.power(1 + raise.power(exp(1),-alpha),3)*raise.power(1 + raise.power(exp(1),-alpha - gamma),3)*(1 + raise.power(exp(1),-alpha - gamma - X*(beta + eta)))*
         ((-1.*raise.power(exp(1),alpha)*(1. + raise.power(exp(1),2*alpha) + 0.5*raise.power(beta,2) + raise.power(exp(1),alpha)*(2. - 0.5*raise.power(beta,2)))*(-1. + omega))/raise.power(1. + raise.power(exp(1),alpha),3) + 
            (raise.power(exp(1),alpha + gamma)*(1. + raise.power(exp(1),2*(alpha + gamma)) + 0.5*raise.power(beta,2) + 1.*beta*eta + 0.5*raise.power(eta,2) + 
                                                  raise.power(exp(1),alpha + gamma)*(2. - 0.5*raise.power(beta,2) - 1.*beta*eta - 0.5*raise.power(eta,2)))*omega)/raise.power(1. + raise.power(exp(1),alpha + gamma),3)))
    
  }

  DR2Gamma <- function(omega, alpha, beta, gamma, eta, X) {
    (-0.5*raise.power(exp(1),alpha + gamma)*(2. + 1.*raise.power(beta,2) + 2.*beta*eta + 1.*raise.power(eta,2) + raise.power(exp(1),alpha + gamma)*(4. - 4.*raise.power(beta,2) - 8.*beta*eta - 4.*raise.power(eta,2)) + 
                                               raise.power(exp(1),2*(alpha + gamma))*(2. + 1.*raise.power(beta,2) + 2.*beta*eta + 1.*raise.power(eta,2)))*omega)/
      (raise.power(1. + raise.power(exp(1),alpha + gamma),4)*((-1.*raise.power(exp(1),alpha)*(1. + raise.power(exp(1),2*alpha) + 0.5*raise.power(beta,2) + raise.power(exp(1),alpha)*(2. - 0.5*raise.power(beta,2)))*(-1. + omega))/
                                                                raise.power(1. + raise.power(exp(1),alpha),3) + (raise.power(exp(1),alpha + gamma)*(1. + raise.power(exp(1),2*(alpha + gamma)) + 0.5*raise.power(beta,2) + 1.*beta*eta + 0.5*raise.power(eta,2) + 
                                                                                                                                                      raise.power(exp(1),alpha + gamma)*(2. - 0.5*raise.power(beta,2) - 1.*beta*eta - 0.5*raise.power(eta,2)))*omega)/raise.power(1. + raise.power(exp(1),alpha + gamma),3)))
    
  }
  
  DR2Eta <- function(omega, alpha, beta, gamma, eta, X) {
    (1.*raise.power(exp(1),alpha + gamma)*(-1 + raise.power(exp(1),alpha + gamma))*(beta + eta)*omega)/
      (raise.power(1 + raise.power(exp(1),alpha + gamma),3)*((-1.*raise.power(exp(1),alpha)*(1. + raise.power(exp(1),2*alpha) + 0.5*raise.power(beta,2) + raise.power(exp(1),alpha)*(2. - 0.5*raise.power(beta,2)))*(-1. + omega))/
                                                               raise.power(1. + raise.power(exp(1),alpha),3) + (raise.power(exp(1),alpha + gamma)*(1. + raise.power(exp(1),2*(alpha + gamma)) + 0.5*raise.power(beta,2) + 1.*beta*eta + 0.5*raise.power(eta,2) + 
                                                                                                                                                     raise.power(exp(1),alpha + gamma)*(2. - 0.5*raise.power(beta,2) - 1.*beta*eta - 0.5*raise.power(eta,2)))*omega)/raise.power(1. + raise.power(exp(1),alpha + gamma),3)))
    
  }

  DR3Gamma <- function(omega, alpha, beta, gamma, eta, X) {
  }
  
  DR3Eta <- function(omega, alpha, beta, gamma, eta, X) {
  }
  
  DR4Gamma <- function(omega, alpha, beta, gamma, eta, X) {
  }
  
  DR4Eta <- function(omega, alpha, beta, gamma, eta, X) {
  }
  
  GRPSUM <- function(omega, alpha, beta, gamma, eta, X, Y, G, f1, f2, f3, f4, cases.only = T) {
    grp1 <- which(Y == 1 & G == 1)
    grp2 <- which(Y == 1 & G == 0)
    
    cases.only.sum <- sum(
      sum(f1(omega, alpha, beta, gamma, eta, X[grp1])),
      sum(f2(omega, alpha, beta, gamma, eta, X[grp2]))
    )
    
    if (cases.only) {
      return(cases.only.sum)
    }
    
    grp3 <- which(Y == 0 & G == 1)
    grp4 <- which(Y == 0 & G == 0)
    
    sum(
      cases.only.sum,
      sum(f3(omega, alpha, beta, gamma, eta, X[grp3])),
      sum(f4(omega, alpha, beta, gamma, eta, X[grp4]))
    )
  }
  
  DGamma <- function(omega, alpha, beta, gamma, eta, X, Y, G, cases.only = T) {
    GRPSUM(omega, alpha, beta, gamma, eta, X, Y, G, DR1Gamma, DR2Gamma, DR3Gamma, DR4Gamma, cases.only)
  }
  
  DEta <- function(omega, alpha, beta, gamma, eta, X, Y, G, cases.only = T) {
    GRPSUM(omega, alpha, beta, gamma, eta, X, Y, G, DR1Eta, DR2Eta, DR3Eta, DR4Eta, cases.only)
  }
  
  ll.alt <- function(omega, alpha, beta, gamma, eta, X, Y, G, cases.only) {
    GRPSUM(omega, alpha, beta, gamma, eta, X, Y, G, lR1, lR2, lR3, lR4, cases.only)
  }
  
  ll.null <- function(alpha, beta, X, Y, cases.only) {
    sum(rXY1(alpha, beta, X[which(Y == 1)])) + sum(ifelse(cases.only, 0, rXY0(alpha, beta, X[which(Y == 0)])))
  }
  
  L.ALT <- function(params, omega, alpha, beta, X, Y, G, cases.only) {
    gamma <- params[1]
    eta <- params[2]
    
    ll.alt(omega, alpha, beta, gamma, eta, X, Y, G, cases.only)
  }
  
  LR <- function(omega, alpha, beta, gamma, eta, X, Y, G, cases.only = T) {
    alt.sum <- GRPSUM(omega, alpha, beta, gamma, eta, X, Y, G, lR1, lR2, lR3, lR4, cases.only)
    null.sum <- sum(rXY1(alpha, beta, X[which(Y == 1)])) + sum(ifelse(cases.only, 0, rXY0(alpha, beta, X[which(Y == 0)])))
    
    alt.sum - null.sum
  }
  
  L <- function(params, omega, alpha, beta, X, Y, G, cases.only) {
    gamma <- params[1]
    eta <- params[2]
    
    LR(omega, alpha, beta, gamma, eta, X, Y, G, cases.only)
  }
  
  D <- function(params, omega, alpha, beta, X, Y, G, cases.only) {
    gamma <- params[1]
    eta <- params[2]
    c(
      # cases only cumulative derivative wrt gamma
      DGamma(omega, alpha, beta, gamma, eta, X, Y, G, cases.only),
      # cases only cumulative derivative wrt eta
      DEta(omega, alpha, beta, gamma, eta, X, Y, G, cases.only)
    )
  }
  
  fisher.info <- function(omega, alpha, beta, gamma, eta, X, Y, G) {
    calc.I <- function(omega, alpha, beta, gamma, eta, X, dGamma, dEta) {
      f.params <- list(
        omega = omega,
        alpha = alpha,
        beta = beta,
        gamma = gamma,
        eta = eta,
        X = X
      )
      g <- f.call(dGamma, f.params)
      e <- f.call(dEta, f.params)
      outer.product <- mapply(function(x, y) outer(c(x,y), c(x,y)), g, e, SIMPLIFY=F)
      Reduce("+", outer.product) / length(outer.product)
    }
    
    grp1 <- which(Y == 1 & G == 1)
    grp2 <- which(Y == 1 & G == 0)
    # grp3 <- which(Y == 0 & G == 1)
    # grp4 <- which(Y == 0 & G == 0)
    
    I1 <- calc.I(omega, alpha, beta, gamma, eta, X[grp1], DR1Gamma, DR1Eta)
    I2 <- calc.I(omega, alpha, beta, gamma, eta, X[grp2], DR2Gamma, DR2Eta)
    # I3 <- calc.I(omega, alpha, beta, gamma, eta, X[grp3], DR3Gamma, DR3Eta)
    # I4 <- calc.I(omega, alpha, beta, gamma, eta, X[grp4], DR4Gamma, DR4Eta)
    
    list(
      n1 = length(grp1),
      I1 = I1,
      n2 = length(grp2),
      I2 = I2
      # n3 = length(grp3),
      # I3 = I3,
      # n4 = length(grp4),
      # I4 = I4
    )
  }
  
  list(
    sim = sim,
    # R1 = R1,
    # R2 = R2,
    # R3 = R3,
    # R4 = R4,
    # RXY1 = RXY1,
    # RXY0 = RXY0,
    # DR1Gamma = DR1Gamma,
    # DR1Eta = DR1Eta,
    # DR2Gamma = DR2Gamma,
    # DR2Eta = DR2Eta,
    # DGamma = DGamma,
    # DEta = DEta,
    LR = LR,
    # L.ALT = L.ALT,
    # L.NULL = L.NULL,
    L = L.ALT,
    D = D,
    fisher.info = fisher.info
  )
}

lrt.logitXG.equations <- define.equations.logitXG()
rm(define.equations.logitXG)
  
  # ??? ####
  # fY1Full <- function(alpha, beta, gamma, eta, omega) {
  #   term1 <- (1 - omega) / (1 + exp(-alpha - beta * ((2 * beta^2) / (exp(2 * alpha) * (1 + exp(-alpha))^3) - beta^2 / (exp(alpha) * (1 + exp(-alpha))^2))))
  #   term2 <- omega / (1 + exp(-alpha - gamma - (beta * ((2 * exp(-2 * alpha - 2 * gamma) * (-beta - eta)^2) / (1 + exp(-alpha - gamma))^3 - (exp(-alpha - gamma) * (-beta - eta)^2) / (1 + exp(-alpha - gamma))^2)) / 2 - (((2 * exp(-2 * alpha - 2 * gamma) * (-beta - eta)^2) / (1 + exp(-alpha - gamma))^3 - (exp(-alpha - gamma) * (-beta - eta)^2) / (1 + exp(-alpha - gamma))^2) * eta) / 2))
  #   return(term1 + term2)
  # }
  # 
  # fY0Full <- function(alpha, beta, omega, gamma, eta) {
  #   exp_alpha <- exp(alpha)
  #   exp_minus_alpha <- exp(-alpha)
  #   exp_minus_alpha_gamma <- exp(-alpha - gamma)
  #   
  #   term1_inner <- (-2 * beta^2) / (exp_alpha^2 * (1 + exp_minus_alpha)^3) + beta^2 / (exp_alpha * (1 + exp_minus_alpha)^2)
  #   term1 <- 1 - 1 / (1 + exp(-alpha - (beta * term1_inner) / 2))
  #   
  #   term2_inner1 <- (-2 * exp(-2 * alpha - 2 * gamma) * (-beta - eta)^2) / (1 + exp_minus_alpha_gamma)^3
  #   term2_inner2 <- (exp_minus_alpha_gamma * (-beta - eta)^2) / (1 + exp_minus_alpha_gamma)^2
  #   term2_inner <- term2_inner1 + term2_inner2
  #   term2_eta_correction <- (term2_inner * eta) / 2
  #   term2 <- 1 - 1 / (1 + exp(-alpha - gamma - (beta * term2_inner) / 2 - term2_eta_correction))
  #   
  #   result <- term1 * (1 - omega) + term2 * omega
  #   
  #   return(result)
  # }
  # fY1X <- function(alpha, beta, X) {
  #   return(1 / (1 + exp(-alpha - X * beta)))
  # }
  # 
  # fY0X <- function(alpha, beta, X) {
  #   return(1 - 1 / (1 + exp(-alpha - X * beta)))
  # }
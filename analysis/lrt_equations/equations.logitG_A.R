define.logitG_A.equations <- function(cases.only=T) {
  raise.power <- function(base, exponent) {
    base ** exponent
  }
  
  fY1XG1 <- function(alpha, beta, gamma, eta, zeta, kappa, X, A) {
  	1/(1 + raise.power(exp(1),-alpha - X*beta - gamma - A*zeta - X*eta - A*X*kappa))
  }
  
  fY1XG0 <- function(alpha, beta, zeta, kappa, X, A) {
  	1/(1 + raise.power(exp(1),-alpha - X*beta - A*zeta - A*X*kappa))
  }
  
  fY0XG1 <- function(alpha, beta, gamma, eta, zeta, kappa, X, A) {
  	1 - 1/(1 + raise.power(exp(1),-alpha - X*beta - gamma - A*zeta - X*eta - A*X*kappa))
  }
  
  fY0XG0 <- function(alpha, beta, zeta, kappa, X, A) {
  	1 - 1/(1 + raise.power(exp(1),-alpha - X*beta - A*zeta - A*X*kappa))
  }
  
  fY1 <- function(omega, alpha, beta, gamma, eta, zeta, kappa, X, A) {
  	(1 - omega)/(1 + raise.power(exp(1),-alpha - X*beta - A*zeta - A*X*kappa)) + omega/(1 + raise.power(exp(1),-alpha - X*beta - gamma - A*zeta - X*eta - A*X*kappa))
  }
  
  fY0 <- function(omega, alpha, beta, gamma, eta, zeta, kappa, X, A) {
  	(1 - 1/(1 + raise.power(exp(1),-alpha - X*beta - A*zeta - A*X*kappa)))*(1 - omega) + (1 - 1/(1 + raise.power(exp(1),-alpha - X*beta - gamma - A*zeta - X*eta - A*X*kappa)))*omega
  }
  
  R1 <- function(omega, alpha, beta, gamma, eta, zeta, kappa, X, A) {
  	1/((1 + raise.power(exp(1),-alpha - X*beta - gamma - A*zeta - X*eta - A*X*kappa))*((1 - omega)/(1 + raise.power(exp(1),-alpha - X*beta - A*zeta - A*X*kappa)) + omega/(1 + raise.power(exp(1),-alpha - X*beta - gamma - A*zeta - X*eta - A*X*kappa))))
  }
  
  R2 <- function(omega, alpha, beta, gamma, eta, zeta, kappa, X, A) {
  	1/((1 + raise.power(exp(1),-alpha - X*beta - A*zeta - A*X*kappa))*((1 - omega)/(1 + raise.power(exp(1),-alpha - X*beta - A*zeta - A*X*kappa)) + omega/(1 + raise.power(exp(1),-alpha - X*beta - gamma - A*zeta - X*eta - A*X*kappa))))
  }
  
  R3 <- function(omega, alpha, beta, gamma, eta, zeta, kappa, X, A) {
  	(1 - 1/(1 + raise.power(exp(1),-alpha - X*beta - gamma - A*zeta - X*eta - A*X*kappa)))/((1 - 1/(1 + raise.power(exp(1),-alpha - X*beta - A*zeta - A*X*kappa)))*(1 - omega) + (1 - 1/(1 + raise.power(exp(1),-alpha - X*beta - gamma - A*zeta - X*eta - A*X*kappa)))*omega)
  }
  
  R4 <- function(omega, alpha, beta, gamma, eta, zeta, kappa, X, A) {
  	(1 - 1/(1 + raise.power(exp(1),-alpha - X*beta - A*zeta - A*X*kappa)))/((1 - 1/(1 + raise.power(exp(1),-alpha - X*beta - A*zeta - A*X*kappa)))*(1 - omega) + (1 - 1/(1 + raise.power(exp(1),-alpha - X*beta - gamma - A*zeta - X*eta - A*X*kappa)))*omega)
  }
  
  DR1Gamma <- function(omega, alpha, beta, gamma, eta, zeta, kappa, X, A) {
  	-((-1 + omega)/(1 + raise.power(exp(1),alpha + gamma + A*zeta + X*(beta + eta + A*kappa)) - omega + raise.power(exp(1),gamma + X*eta)*omega))
  }
  
  DR1Eta <- function(omega, alpha, beta, gamma, eta, zeta, kappa, X, A) {
  	-((X*(-1 + omega))/(1 + raise.power(exp(1),alpha + gamma + A*zeta + X*(beta + eta + A*kappa)) - omega + raise.power(exp(1),gamma + X*eta)*omega))
  }
  
  DR2Gamma <- function(omega, alpha, beta, gamma, eta, zeta, kappa, X, A) {
  	-((raise.power(exp(1),gamma + X*eta)*(1 + raise.power(exp(1),alpha + X*beta + A*zeta + A*X*kappa))*omega)/((1 + raise.power(exp(1),alpha + gamma + A*zeta + X*(beta + eta + A*kappa)))*(1 + raise.power(exp(1),alpha + gamma + A*zeta + X*(beta + eta + A*kappa)) - omega + raise.power(exp(1),gamma + X*eta)*omega)))
  }
  
  DR2Eta <- function(omega, alpha, beta, gamma, eta, zeta, kappa, X, A) {
  	-((raise.power(exp(1),gamma + X*eta)*(1 + raise.power(exp(1),alpha + X*beta + A*zeta + A*X*kappa))*X*omega)/((1 + raise.power(exp(1),alpha + gamma + A*zeta + X*(beta + eta + A*kappa)))*(1 + raise.power(exp(1),alpha + gamma + A*zeta + X*(beta + eta + A*kappa)) - omega + raise.power(exp(1),gamma + X*eta)*omega)))
  }
  
  DR3Gamma <- function(omega, alpha, beta, gamma, eta, zeta, kappa, X, A) {
  	-((raise.power(exp(1),alpha + gamma + A*zeta + X*(beta + eta + A*kappa))*(-1 + omega))/(-1 + raise.power(exp(1),alpha + gamma + A*zeta + X*(beta + eta + A*kappa))*(-1 + omega) - raise.power(exp(1),alpha + X*beta + A*zeta + A*X*kappa)*omega))
  }
  
  DR3Eta <- function(omega, alpha, beta, gamma, eta, zeta, kappa, X, A) {
  	-((raise.power(exp(1),alpha + gamma + A*zeta + X*(beta + eta + A*kappa))*X*(-1 + omega))/(-1 + raise.power(exp(1),alpha + gamma + A*zeta + X*(beta + eta + A*kappa))*(-1 + omega) - raise.power(exp(1),alpha + X*beta + A*zeta + A*X*kappa)*omega))
  }
  
  DR4Gamma <- function(omega, alpha, beta, gamma, eta, zeta, kappa, X, A) {
  	-((raise.power(exp(1),alpha + gamma + A*zeta + X*(beta + eta + A*kappa))*(1 + raise.power(exp(1),alpha + X*beta + A*zeta + A*X*kappa))*omega)/((1 + raise.power(exp(1),alpha + gamma + A*zeta + X*(beta + eta + A*kappa)))*(-1 + raise.power(exp(1),alpha + gamma + A*zeta + X*(beta + eta + A*kappa))*(-1 + omega) - raise.power(exp(1),alpha + X*beta + A*zeta + A*X*kappa)*omega)))
  }
  
  DR4Eta <- function(omega, alpha, beta, gamma, eta, zeta, kappa, X, A) {
  	-((raise.power(exp(1),alpha + gamma + A*zeta + X*(beta + eta + A*kappa))*(1 + raise.power(exp(1),alpha + X*beta + A*zeta + A*X*kappa))*X*omega)/((1 + raise.power(exp(1),alpha + gamma + A*zeta + X*(beta + eta + A*kappa)))*(-1 + raise.power(exp(1),alpha + gamma + A*zeta + X*(beta + eta + A*kappa))*(-1 + omega) - raise.power(exp(1),alpha + X*beta + A*zeta + A*X*kappa)*omega)))
  }
  
  lR1 <- function(omega, alpha, beta, gamma, eta, zeta, kappa, X, A) {
    log(R1(omega, alpha, beta, gamma, eta, zeta, kappa, X, A))
  }
  
  lR2 <- function(omega, alpha, beta, gamma, eta, zeta, kappa, X, A) {
    log(R2(omega, alpha, beta, gamma, eta, zeta, kappa, X, A))
  }
  
  lR3 <- function(omega, alpha, beta, gamma, eta, zeta, kappa, X, A) {
    log(R3(omega, alpha, beta, gamma, eta, zeta, kappa, X, A))
  }
  
  lR4 <- function(omega, alpha, beta, gamma, eta, zeta, kappa, X, A) {
    log(R4(omega, alpha, beta, gamma, eta, zeta, kappa, X, A))
  }

  GRPSUM <- function(omega, alpha, beta, gamma, eta, zeta, kappa, X, Y, G, A, f1, f2, f3, f4) {
    grp1 <- which(Y == 1 & G == 1)
    grp2 <- which(Y == 1 & G == 0)
    
    cases.only.sum <- sum(
      sum(f1(omega, alpha, beta, gamma, eta, zeta, kappa, X[grp1], A[grp1])),
      sum(f2(omega, alpha, beta, gamma, eta, zeta, kappa, X[grp2], A[grp2]))
    )
    
    if (cases.only) {
      return(cases.only.sum)
    }
    
    grp3 <- which(Y == 0 & G == 1)
    grp4 <- which(Y == 0 & G == 0)
    
    sum(
      cases.only.sum,
      sum(f3(omega, alpha, beta, gamma, eta, zeta, kappa, X[grp3], A[grp3])),
      sum(f4(omega, alpha, beta, gamma, eta, zeta, kappa, X[grp4], A[grp4]))
    )
  }
  
  DGamma <- function(omega, alpha, beta, gamma, eta, zeta, kappa, X, Y, G, A) {
    GRPSUM(omega, alpha, beta, gamma, eta, zeta, kappa, X, Y, G, A, DR1Gamma, DR2Gamma, DR3Gamma, DR4Gamma)
  }
  
  DEta <- function(omega, alpha, beta, gamma, eta, zeta, kappa, X, Y, G, A) {
    GRPSUM(omega, alpha, beta, gamma, eta, zeta, kappa, X, Y, G, A, DR1Eta, DR2Eta, DR3Eta, DR4Eta)
  }
  
  LR <- function(omega, alpha, beta, gamma, eta, zeta, kappa, X, Y, G, A) {
    GRPSUM(omega, alpha, beta, gamma, eta, zeta, kappa, X, Y, G, A, lR1, lR2, lR3, lR4)
  }
  
  L <- function(params, omega, alpha, beta, zeta, kappa, X, Y, G, A) {
    gamma <- params[1]
    eta <- params[2]
    
    LR(omega, alpha, beta, gamma, eta, zeta, kappa, X, Y, G, A)
  }
  
  D <- function(params, omega, alpha, beta, zeta, kappa, X, Y, G, A) {
    gamma <- params[1]
    eta <- params[2]
    c(
      # cumulative derivative wrt gamma
      DGamma(omega, alpha, beta, gamma, eta, zeta, kappa, X, Y, G, A),
      # cumulative derivative wrt eta
      DEta(omega, alpha, beta, gamma, eta, zeta, kappa, X, Y, G, A)
    )
  }
  
  fisher.info <- function(omega, alpha, beta, gamma, eta, zeta, kappa, X, Y, G, A) {
    calc.I <- function(omega, alpha, beta, gamma, eta, zeta, kappa, X, A, dGamma, dEta) {
      f.params <- list(
        omega = omega,
        alpha = alpha,
        beta = beta,
        gamma = gamma,
        eta = eta,
        zeta = zeta,
        kappa = kappa,
        X = X,
        A = A
      )
      g <- f.call(dGamma, f.params)
      e <- f.call(dEta, f.params)
      outer.product <- mapply(function(x, y) outer(c(x,y), c(x,y)), g, e, SIMPLIFY=F)
      Reduce("+", outer.product) / length(outer.product)
    }

    grp1 <- which(Y == 1 & G == 1)
    grp2 <- which(Y == 1 & G == 0)
    grp3 <- which(Y == 0 & G == 1)
    grp4 <- which(Y == 0 & G == 0)

    I1 <- calc.I(omega, alpha, beta, gamma, eta, zeta, kappa, X[grp1], A[grp1], DR1Gamma, DR1Eta)
    I2 <- calc.I(omega, alpha, beta, gamma, eta, zeta, kappa, X[grp2], A[grp2], DR2Gamma, DR2Eta)
    I3 <- calc.I(omega, alpha, beta, gamma, eta, zeta, kappa, X[grp3], A[grp3], DR3Gamma, DR3Eta)
    I4 <- calc.I(omega, alpha, beta, gamma, eta, zeta, kappa, X[grp4], A[grp4], DR4Gamma, DR4Eta)

    list(
      n1 = length(grp1),
      I1 = I1,
      n2 = length(grp2),
      I2 = I2,
      n3 = length(grp3),
      I3 = I3,
      n4 = length(grp4),
      I4 = I4
    )
  }
  
  list(
    L = L,
    D = D,
    LR = LR,
    fisher.info = fisher.info
  )
}
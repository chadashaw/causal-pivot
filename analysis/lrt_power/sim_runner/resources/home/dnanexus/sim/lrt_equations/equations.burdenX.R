define.equations <- function() {
  params.0 <- list(
    n=500000,
    alpha=-3,
    beta=0.5,
    gamma=0,
    eta=0,
    lambda=0.083
  )
  
  sim <- function(
    n,
    alpha,
    beta,
    gamma,
    eta,
    omega,
    lambda
  ) {
    G <- rpois(n = n, lambda = lambda)
    
    i <- G < 4
    
    X <- rnorm(n = n, mean = 0, sd = 1)
    
    GX <- G * X
    
    pY <- expit(alpha + beta * X + gamma * G + eta * GX)
    
    Y <- rbinom(n = n, size = 1, prob = pY)
    
    list(
      Y = Y[i],
      X = X[i],
      G = G[i],
      GX = GX[i]
    )
  }
  
  fY1XG <- function(alpha, beta, gamma, eta, X, G) {
    1 / (1 + exp(-1 * (alpha + beta * X + gamma * G + eta * X * G)))
  }
  
  ddfY1X0G <- function(alpha, beta, gamma, eta, G) {
    term1 <- exp(alpha + G * gamma)
    term2 <- (beta + G * eta) ^ 2
    
    numerator <- term1 * (-1 + term1) * term2
    denominator <- (1 + term1) ^ 3
    
    -1 * numerator/ denominator
  }
  
  RXY1 <- function(alpha, beta, X) {
    fY1XG(alpha, beta, 0, 0, X, 0) /
      (fY1XG(alpha, beta, 0, 0, 0, 0) + ddfY1X0G(alpha, beta, 0, 0, 0) / 2)
  }
  
  lRXY1 <- function(alpha, beta, X) {
    log(RXY1(alpha, beta, X))
  }
  
  RY1G <- function(alpha, beta, gamma, eta, X, G) {
    fY1XG(alpha, beta, gamma, eta, X, G) /
      (fY1XG(alpha, beta, gamma, eta, 0, G) + ddfY1X0G(alpha, beta, gamma, eta, G) / 2)
  }
  
  lRY1G <- function(alpha, beta, gamma, eta, X, G) {
    log(RY1G(alpha, beta, gamma, eta, X, G))
  }
  
  DY1G0Gamma <- function(alpha, beta, gamma, eta, X) {
    0
  }
  
  DY1G1Gamma <- function(alpha, beta, gamma, eta, X) {
    # Calculate the exponential terms
    exp_term_1 <- exp(-alpha - X * beta - gamma - X * eta)
    exp_term_2 <- exp(-alpha - gamma)
    exp_term_3 <- exp(-2 * alpha - 2 * gamma)
    exp_term_4 <- exp(-3 * alpha - 3 * gamma)
    
    # Common denominator
    common_denom_1 <- 1 / (1 + exp_term_2) +
      (1 / 2) * ((2 * exp_term_3 * (-beta - eta)^2) / (1 + exp_term_2)^3 -
      (exp_term_2 * (-beta - eta)^2) / (1 + exp_term_2)^2)
    
    denom_1 <- 1 + exp_term_1
    denom_2 <- 1 + exp_term_2
    
    # First part of the derivative
    term_1 <- exp_term_1 / (denom_1^2 * common_denom_1)
    
    # Second part of the derivative
    term_2 <- exp_term_2 / denom_2^2 +
      (1/2) * ((6 * exp_term_4 * (-beta - eta)^2) / denom_2^4 -
      (6 * exp_term_3 * (-beta - eta)^2) / denom_2^3 +
      exp_term_2 * (-beta - eta)^2 / denom_2^2)

    common_denom_2 <- denom_1 * (common_denom_1)^2
    
    # Final derivative result
    denom_1 * (term_1 - term_2 / common_denom_2) * common_denom_1
  }
  
  DY1G2Gamma <- function(alpha, beta, gamma, eta, X) {
    # Calculate the exponential terms
    exp_term_1 <- exp(-alpha - X * beta - 2 * gamma - 2 * X * eta)
    exp_term_2 <- exp(-alpha - 2 * gamma)
    exp_term_3 <- exp(-2 * alpha - 4 * gamma)
    exp_term_4 <- exp(-3 * alpha - 6 * gamma)
    
    # Common denominator term
    common_denom_1 <- 1 / (1 + exp_term_2) +
      (1/2) * ((2 * exp_term_3 * (-beta - 2 * eta)^2) / (1 + exp_term_2)^3 -
      (exp_term_2 * (-beta - 2 * eta)^2) / (1 + exp_term_2)^2)
    
    denom_1 <- 1 + exp_term_1
    denom_2 <- 1 + exp_term_2
    
    # First part of the derivative
    term_1 <- (2 * exp_term_1) / (denom_1^2 * common_denom_1)
    
    # Second part of the derivative
    term_2 <- (2 * exp_term_2) / denom_2^2 +
      (1 / 2) * ((12 * exp_term_4 * (-beta - 2 * eta)^2) / denom_2^4 -
      (12 * exp_term_3 * (-beta - 2 * eta)^2) / denom_2^3 +
      (2 * exp_term_2 * (-beta - 2 * eta)^2) / denom_2^2)
    
    common_denom_2 <- denom_1 * common_denom_1^2
    
    # Final derivative result
    denom_1 * (term_1 - term_2 / common_denom_2) * common_denom_1
  }
  
  DY1G3Gamma <- function(alpha, beta, gamma, eta, X) {
    # Calculate the exponential terms
    exp_term_1 <- exp(-alpha - X * beta - 3 * gamma - 3 * X * eta)
    exp_term_2 <- exp(-alpha - 3 * gamma)
    exp_term_3 <- exp(-2 * alpha - 6 * gamma)
    exp_term_4 <- exp(-3 * alpha - 9 * gamma)
    
    # Common denominator term
    common_denom_1 <- 1 / (1 + exp_term_2) +
      (1 / 2) * ((2 * exp_term_3 * (-beta - 3 * eta)^2) / (1 + exp_term_2)^3 -
      (exp_term_2 * (-beta - 3 * eta)^2) / (1 + exp_term_2)^2)
    
    denom_1 <- 1 + exp_term_1
    denom_2 <- 1 + exp_term_2
    
    # First part of the derivative
    term_1 <- (3 * exp_term_1) / (denom_1^2 * common_denom_1)
    
    # Second part of the derivative
    term_2 <- (3 * exp_term_2) / denom_2^2 +
      (1 / 2) * ((18 * exp_term_4 * (-beta - 3 * eta)^2) / denom_2^4 -
      (18 * exp_term_3 * (-beta - 3 * eta)^2) / denom_2^3 +
      (3 * exp_term_2 * (-beta - 3 * eta)^2) / denom_2^2)
    
    common_denom_2 <- denom_1 * common_denom_1^2
    
    # Final derivative result
    denom_1 * (term_1 - term_2 / common_denom_2) * common_denom_1
  }
  
  DY1G0Eta <- function(alpha, beta, gamma, eta, X) {
    0
  }
  
  DY1G1Eta <- function(alpha, beta, gamma, eta, X) {
    # Calculate the exponential terms
    exp_term_1 <- exp(-alpha - X * beta - gamma - X * eta)
    exp_term_2 <- exp(-alpha - gamma)
    exp_term_3 <- exp(-2 * alpha - 2 * gamma)
    
    # Common denominator term
    common_denom_1 <- 1 / (1 + exp_term_2) +
      (1 / 2) * ((2 * exp_term_3 * (-beta - eta)^2) / (1 + exp_term_2)^3 -
      (exp_term_2 * (-beta - eta)^2) / (1 + exp_term_2)^2)
    
    denom_1 <- 1 + exp_term_1
    denom_2 <- 1 + exp_term_2
    
    # First part of the derivative
    term_1 <- (exp_term_1 * X) / (denom_1^2 * common_denom_1)
    
    # Second part of the derivative
    term_2 <- (-4 * exp_term_3 * (-beta - eta)) / denom_2^3 +
      (2 * exp_term_2 * (-beta - eta)) / denom_2^2
    
    common_denom_2 <- denom_1 * common_denom_1^2
    
    # Final derivative result
    denom_1 * (term_1 - term_2 / (2 * common_denom_2)) * common_denom_1
  }
  
  DY1G2Eta <- function(alpha, beta, gamma, eta, X) {
    # Calculate the exponential terms
    exp_term_1 <- exp(-alpha - X * beta - 2 * gamma - 2 * X * eta)
    exp_term_2 <- exp(-alpha - 2 * gamma)
    exp_term_3 <- exp(-2 * alpha - 4 * gamma)
    
    # Common denominator term
    common_denom_1 <- 1 / (1 + exp_term_2) +
      (1 / 2) * ((2 * exp_term_3 * (-beta - 2 * eta)^2) / (1 + exp_term_2)^3 -
      (exp_term_2 * (-beta - 2 * eta)^2) / (1 + exp_term_2)^2)
    
    denom_1 <- 1 + exp_term_1
    denom_2 <- 1 + exp_term_2
    
    # First part of the derivative
    term_1 <- (2 * exp_term_1 * X) / (denom_1^2 * common_denom_1)
    
    # Second part of the derivative
    term_2 <- (-8 * exp_term_3 * (-beta - 2 * eta)) / denom_2^3 +
      (4 * exp_term_2 * (-beta - 2 * eta)) / denom_2^2
    
    common_denom_2 <- denom_1 * common_denom_1^2
    
    # Final derivative result
    denom_1 * (term_1 - term_2 / (2 * common_denom_2)) * common_denom_1
  }
  
  DY1G3Eta <- function(alpha, beta, gamma, eta, X) {
    # Calculate the exponential terms
    exp_term_1 <- exp(-alpha - X * beta - 3 * gamma - 3 * X * eta)
    exp_term_2 <- exp(-alpha - 3 * gamma)
    exp_term_3 <- exp(-2 * alpha - 6 * gamma)
    
    # Common denominator term
    common_denom_1 <- 1 / (1 + exp_term_2) +
      (1 / 2) * ((2 * exp_term_3 * (-beta - 3 * eta)^2) / (1 + exp_term_2)^3 -
      (exp_term_2 * (-beta - 3 * eta)^2) / (1 + exp_term_2)^2)
    
    denom_1 <- 1 + exp_term_1
    denom_2 <- 1 + exp_term_2
    
    # First part of the derivative
    term_1 <- (3 * exp_term_1 * X) / (denom_1^2 * common_denom_1)
    
    # Second part of the derivative
    term_2 <- (-12 * exp_term_3 * (-beta - 3 * eta)) / denom_2^3 +
      (6 * exp_term_2 * (-beta - 3 * eta)) / denom_2^2
    
    common_denom_2 <- denom_1 * common_denom_1^2
    
    # Final derivative result
    denom_1 * (term_1 - term_2 / (2 * common_denom_2)) * common_denom_1
  }
  
  GRPSUM <- function(alpha, beta, gamma, eta, X, Y, G, f0, f1, f2, f3, cases.only = T) {
    
    grp0 <- which(Y == 1 & G == 0)
    grp1 <- which(Y == 1 & G == 1)
    grp2 <- which(Y == 1 & G == 2)
    grp3 <- which(Y == 1 & G == 3)
    
    cases.only.sum <- sum(
      sum(f0(alpha, beta, gamma, eta, X[grp0])),
      sum(f1(alpha, beta, gamma, eta, X[grp1])),
      sum(f2(alpha, beta, gamma, eta, X[grp2])),
      sum(f3(alpha, beta, gamma, eta, X[grp3]))
    )
    
    if (cases.only) {
      return(cases.only.sum)
    }
    
    stop('cases + controls unimplemented')
  }
  
  DGamma <- function(alpha, beta, gamma, eta, X, Y, G, cases.only = T) {
    GRPSUM(alpha, beta, gamma, eta, X, Y, G, DY1G0Gamma, DY1G1Gamma, DY1G2Gamma, DY1G3Gamma, cases.only)
  }
  
  DEta <- function(alpha, beta, gamma, eta, X, Y, G, cases.only = T) {
    GRPSUM(alpha, beta, gamma, eta, X, Y, G, DY1G0Eta, DY1G1Eta, DY1G2Eta, DY1G3Eta, cases.only)
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
  
  LL <- function(alpha, beta, gamma, eta, X, Y, G, cases.only = T) {
    lR0 <- function(alpha, beta, gamma, eta, X) lRY1G(alpha, beta, gamma, eta, X, 0)
    lR1 <- function(alpha, beta, gamma, eta, X) lRY1G(alpha, beta, gamma, eta, X, 1)
    lR2 <- function(alpha, beta, gamma, eta, X) lRY1G(alpha, beta, gamma, eta, X, 2)
    lR3 <- function(alpha, beta, gamma, eta, X) lRY1G(alpha, beta, gamma, eta, X, 3)
    
    alt.sum <- GRPSUM(alpha, beta, gamma, eta, X, Y, G, lR0, lR1, lR2, lR3, cases.only)
    null.sum <- sum(lRXY1(alpha, beta, X[which(Y == 1)])) + sum(ifelse(cases.only, 0, stop('cases + controls unimplemented')))
    
    list(
      alt = alt.sum,
      null = null.sum,
      ll = alt.sum - null.sum
    )
  }
  
  list(
    params.0 = params.0,
    sim = sim,
    D = D,
    LL = LL
  )
}

equations <- define.equations()
rm(define.equations)

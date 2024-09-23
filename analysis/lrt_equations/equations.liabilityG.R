define.liabilityG.equations <- function() {
  library(pracma, quietly = T, warn.conflicts = F) # for erfc()

  sim <- function(
    n,
    alpha,
    beta,
    gamma,
    eta,
    omega,
    sige
  ) {
    G <- rbinom(n = n, size = 1, prob = omega)
    
    X <- rnorm(n = n, mean = 0, sd = 1)
    
    GX <- G * X
    
    uY <- alpha + beta * X + gamma * G + eta * GX
    
    y <- rnorm(n, mean = uY, sd = sige)
    
    list(
      y = y,
      X = X,
      G = G,
      GX = GX
    )
  }
  
  uyG1 <- function(alpha, beta, gamma, eta, X) {
    alpha + beta * X + gamma + X * eta
  }
  
  uyG0 <- function(alpha, beta, X) {
    alpha + X * beta
  }
  
  FLupperG1 <- function(alpha, beta, gamma, eta, delta, sige, X, y) {
    u <- uyG1(alpha, beta, gamma, eta, X)
    dnorm(y, u, sige) / (1 - pnorm(delta, u, sige))
  }
  
  FLupperG0 <- function(alpha, beta, delta, sige, X, y) {
    u <- uyG0(alpha, beta, X)
    dnorm(y, u, sige) / (1 - pnorm(delta, u, sige))
  }
  
  FLlowerG1 <- function(alpha, beta, gamma, eta, delta, sige, X, y) {
    u <- uyG1(alpha, beta, gamma, eta, X)
    dnorm(y, u, sige) / (pnorm(delta, u, sige))
  }
  
  FLlowerG0 <- function(alpha, beta, delta, sige, X, y) {
    u <- uyG0(alpha, beta, X)
    dnorm(y, u, sige) / (pnorm(delta, u, sige))
  }
  
  denomUpper <- function(omega, alpha, beta, gamma, eta, delta, sige, X, y) {
    g1 <- FLupperG1(alpha, beta, gamma, eta, delta, sige, X, y)
    g0 <- FLupperG0(alpha, beta, delta, sige, X, y)
    omega * g1 + (1 - omega) * g0
  }
  
  denomLower <- function(omega, alpha, beta, gamma, eta, delta, sige, X, y) {
    g1 <- FLlowerG1(alpha, beta, gamma, eta, delta, sige, X, y)
    g0 <- FLlowerG0(alpha, beta, delta, sige, X, y)
    omega * g1 + (1 - omega) * g0
  }
  
  R1 <- function(omega, alpha, beta, gamma, eta, delta, sige, X, y) {
    FLupperG1(alpha, beta, gamma, eta, delta, sige, X, y) /
      denomUpper(omega, alpha, beta, gamma, eta, delta, sige, X, y)
  }
  
  R2 <- function(omega, alpha, beta, gamma, eta, delta, sige, X, y) {
    FLupperG0(alpha, beta, delta, sige, X, y) /
      denomUpper(omega, alpha, beta, gamma, eta, delta, sige, X, y)
  }
  
  R3 <- function(omega, alpha, beta, gamma, eta, delta, sige, X, y) {
    FLlowerG1(alpha, beta, gamma, eta, delta, sige, X, y) /
      denomLower(omega, alpha, beta, gamma, eta, delta, sige, X, y)
  }
  
  R4 <- function(omega, alpha, beta, gamma, eta, delta, sige, X, y) {
    FLlowerG0(alpha, beta, delta, sige, X, y) /
      denomLower(omega, alpha, beta, gamma, eta, delta, sige, X, y)
  }
  
  lR1 <- function(omega, alpha, beta, gamma, eta, delta, sige, X, y) {
    log(R1(omega, alpha, beta, gamma, eta, delta, sige, X, y))
  }
  
  lR2 <- function(omega, alpha, beta, gamma, eta, delta, sige, X, y) {
    log(R2(omega, alpha, beta, gamma, eta, delta, sige, X, y))
  }
  
  lR3 <- function(omega, alpha, beta, gamma, eta, delta, sige, X, y) {
    log(R3(omega, alpha, beta, gamma, eta, delta, sige, X, y))
  }
  
  lR4 <- function(omega, alpha, beta, gamma, eta, delta, sige, X, y) {
    log(R4(omega, alpha, beta, gamma, eta, delta, sige, X, y))
  }
  
  DR1Gamma <- function(omega, alpha, beta, gamma, eta, delta, sige, X, y) {
    term_exp1 <- exp((y - delta) * (y - 2 * alpha - 2 * X * beta - 2 * gamma + delta - 2 * X * eta) / (2 * sige^2))
    term_exp2 <- exp((alpha + gamma - delta + X * (beta + eta))^2 / (2 * sige^2))
    term_exp3 <- exp((-y + alpha + X * beta + gamma + X * eta)^2 / (2 * sige^2))
    term_exp4 <- exp((-y + alpha + X * beta)^2 / (2 * sige^2))
    
    numerator <- term_exp1 * (-1 + omega) * (
      2 * (sige + term_exp2 * sqrt(2 * pi) * (-y + alpha + X * beta + gamma + X * eta)) -
        term_exp2 * sqrt(2 * pi) * (-y + alpha + X * beta + gamma + X * eta) * 
        erfc((alpha + gamma - delta + X * (beta + eta)) / (sqrt(2) * sige))
    )
    
    denominator <- sqrt(2 * pi) * sige^2 * (
      -2 * term_exp3 * (-1 + omega) + 
        2 * term_exp4 * omega - 
        term_exp4 * omega * erfc((alpha + X * beta - delta) / (sqrt(2) * sige)) + 
        term_exp3 * (-1 + omega) * erfc((alpha + gamma - delta + X * (beta + eta)) / (sqrt(2) * sige))
    )
    
    numerator / denominator
  }
  
  DR1Eta <- function(omega, alpha, beta, gamma, eta, delta, sige, X, y) {
    term_exp1 <- exp((y - delta) * (y - 2 * alpha - 2 * X * beta - 2 * gamma + delta - 2 * X * eta) / (2 * sige^2))
    term_exp2 <- exp((alpha + gamma - delta + X * (beta + eta))^2 / (2 * sige^2))
    term_exp3 <- exp((-y + alpha + X * beta + gamma + X * eta)^2 / (2 * sige^2))
    term_exp4 <- exp((-y + alpha + X * beta)^2 / (2 * sige^2))
    
    numerator <- term_exp1 * X * (-1 + omega) * (
      -2 * (sige + term_exp2 * sqrt(2 * pi) * (-y + alpha + X * beta + gamma + X * eta)) +
        term_exp2 * sqrt(2 * pi) * (-y + alpha + X * beta + gamma + X * eta) * 
        erfc((alpha + gamma - delta + X * (beta + eta)) / (sqrt(2) * sige))
    )
    
    denominator <- sqrt(2 * pi) * sige^2 * (
      -2 * term_exp3 * (-1 + omega) + 
        2 * term_exp4 * omega - 
        term_exp4 * omega * erfc((alpha + X * beta - delta) / (sqrt(2) * sige)) + 
        term_exp3 * (-1 + omega) * erfc((alpha + gamma - delta + X * (beta + eta)) / (sqrt(2) * sige))
    )
    
    - numerator / denominator
  }
  
  DR2Gamma <- function(omega, alpha, beta, gamma, eta, delta, sige, X, y) {
    term1 <- exp((alpha + gamma - delta + X * (beta + eta))^2 / (2 * sige^2))
    term2 <- exp((-y + alpha + X * beta + gamma + X * eta)^2 / (2 * sige^2))
    term3 <- exp((-y + alpha + X * beta)^2 / (2 * sige^2))
    
    numerator <- -2 * sqrt(2 * pi) * omega * (
      -(sige / (term1 * pi)) - 
        ((y - alpha - X * beta - gamma - X * eta) * 
           (-2 + erfc((alpha + gamma - delta + X * (beta + eta)) / (sqrt(2) * sige)))) / 
        sqrt(2 * pi)
    )
    
    denominator <- term2 * sige^2 * (
      (2 * (-1 + omega)) / 
        (term3 * (-2 + erfc((alpha + X * beta - delta) / (sqrt(2) * sige)))) - 
        (2 * omega) / 
        (term2 * (-2 + erfc((alpha + gamma - delta + X * (beta + eta)) / (sqrt(2) * sige))))
    ) * 
      (-2 + erfc((alpha + gamma - delta + X * (beta + eta)) / (sqrt(2) * sige)))^2
    
    numerator / denominator
  }
  
  DR2Eta <- function(omega, alpha, beta, gamma, eta, delta, sige, X, y) {
    term1 <- exp((alpha + gamma - delta + X * (beta + eta))^2 / (2 * sige^2))
    term2 <- exp((-y + alpha + X * beta + gamma + X * eta)^2 / (2 * sige^2))
    term3 <- exp((-y + alpha + X * beta)^2 / (2 * sige^2))
    
    numerator <- -2 * sqrt(2 * pi) * X * omega * (
      -(sige / (term1 * pi)) - 
        ((y - alpha - X * beta - gamma - X * eta) * 
           (-2 + erfc((alpha + gamma - delta + X * (beta + eta)) / (sqrt(2) * sige)))) / 
        sqrt(2 * pi)
    )
    
    denominator <- term2 * sige^2 * (
      (2 * (-1 + omega)) / 
        (term3 * (-2 + erfc((alpha + X * beta - delta) / (sqrt(2) * sige)))) - 
        (2 * omega) / 
        (term2 * (-2 + erfc((alpha + gamma - delta + X * (beta + eta)) / (sqrt(2) * sige))))
    ) * 
      (-2 + erfc((alpha + gamma - delta + X * (beta + eta)) / (sqrt(2) * sige)))^2
    
    numerator / denominator
  }
  
  DR3Gamma <- function(omega, alpha, beta, gamma, eta, delta, sige, X, y) {
    term_exp1 <- exp((y - delta) * (y - 2 * alpha - 2 * X * beta - 2 * gamma + delta - 2 * X * eta) / (2 * sige^2))
    term_exp2 <- exp((alpha + gamma - delta + X * (beta + eta))^2 / (2 * sige^2))
    term_exp3 <- exp((-y + alpha + X * beta + gamma + X * eta)^2 / (2 * sige^2))
    term_exp4 <- exp((-y + alpha + X * beta)^2 / (2 * sige^2))
    
    numerator <- -term_exp1 * (-1 + omega) * (
      2 * sige - term_exp2 * sqrt(2 * pi) * (-y + alpha + X * beta + gamma + X * eta) * 
        erfc((alpha + gamma - delta + X * (beta + eta)) / (sqrt(2) * sige))
    )
    
    denominator <- sqrt(2 * pi) * sige^2 * (
      term_exp4 * omega * erfc((alpha + X * beta - delta) / (sqrt(2) * sige)) - 
        term_exp3 * (-1 + omega) * erfc((alpha + gamma - delta + X * (beta + eta)) / (sqrt(2) * sige))
    )
    
    numerator / denominator
  }
  
  DR3Eta <- function(omega, alpha, beta, gamma, eta, delta, sige, X, y) {
    term_exp1 <- exp((y - delta) * (y - 2 * alpha - 2 * X * beta - 2 * gamma + delta - 2 * X * eta) / (2 * sige^2))
    term_exp2 <- exp((alpha + gamma - delta + X * (beta + eta))^2 / (2 * sige^2))
    term_exp3 <- exp((-y + alpha + X * beta + gamma + X * eta)^2 / (2 * sige^2))
    term_exp4 <- exp((-y + alpha + X * beta)^2 / (2 * sige^2))
    
    numerator <- term_exp1 * X * (-1 + omega) * (
      -2 * sige + term_exp2 * sqrt(2 * pi) * (-y + alpha + X * beta + gamma + X * eta) * 
        erfc((alpha + gamma - delta + X * (beta + eta)) / (sqrt(2) * sige))
    )
    
    denominator <- sqrt(2 * pi) * sige^2 * (
      term_exp4 * omega * erfc((alpha + X * beta - delta) / (sqrt(2) * sige)) - 
        term_exp3 * (-1 + omega) * erfc((alpha + gamma - delta + X * (beta + eta)) / (sqrt(2) * sige))
    )
    
    numerator / denominator
  }
  
  DR4Gamma <- function(omega, alpha, beta, gamma, eta, delta, sige, X, y) {
    term_exp1 <- exp((y - 2 * alpha - 2 * X * beta - gamma + delta - X * eta) * 
                       (y + gamma - delta + X * eta) / (2 * sige^2))
    term_exp2 <- exp((alpha + gamma - delta + X * (beta + eta))^2 / (2 * sige^2))
    term_exp3 <- exp((-y + alpha + X * beta + gamma + X * eta)^2 / (2 * sige^2))
    term_exp4 <- exp((-y + alpha + X * beta)^2 / (2 * sige^2))
    
    numerator <- -term_exp1 * omega * erfc((alpha + X * beta - delta) / (sqrt(2) * sige)) * 
      (2 * sige - term_exp2 * sqrt(2 * pi) * (-y + alpha + X * beta + gamma + X * eta) * 
         erfc((alpha + gamma - delta + X * (beta + eta)) / (sqrt(2) * sige)))
    
    denominator <- sqrt(2 * pi) * sige^2 * erfc((alpha + gamma - delta + X * (beta + eta)) / (sqrt(2) * sige)) * 
      (term_exp4 * omega * erfc((alpha + X * beta - delta) / (sqrt(2) * sige)) - 
         term_exp3 * (-1 + omega) * erfc((alpha + gamma - delta + X * (beta + eta)) / (sqrt(2) * sige)))
    
    numerator / denominator
  }
  
  DR4Eta <- function(omega, alpha, beta, gamma, eta, delta, sige, X, y) {
    term_exp1 <- exp((y - 2 * alpha - 2 * X * beta - gamma + delta - X * eta) * 
                       (y + gamma - delta + X * eta) / (2 * sige^2))
    term_exp2 <- exp((alpha + gamma - delta + X * (beta + eta))^2 / (2 * sige^2))
    term_exp3 <- exp((-y + alpha + X * beta + gamma + X * eta)^2 / (2 * sige^2))
    term_exp4 <- exp((-y + alpha + X * beta)^2 / (2 * sige^2))
    
    numerator <- term_exp1 * X * omega * erfc((alpha + X * beta - delta) / (sqrt(2) * sige)) * 
      (-2 * sige + term_exp2 * sqrt(2 * pi) * (-y + alpha + X * beta + gamma + X * eta) * 
         erfc((alpha + gamma - delta + X * (beta + eta)) / (sqrt(2) * sige)))
    
    denominator <- sqrt(2 * pi) * sige^2 * erfc((alpha + gamma - delta + X * (beta + eta)) / (sqrt(2) * sige)) * 
      (term_exp4 * omega * erfc((alpha + X * beta - delta) / (sqrt(2) * sige)) - 
         term_exp3 * (-1 + omega) * erfc((alpha + gamma - delta + X * (beta + eta)) / (sqrt(2) * sige)))
    
    numerator / denominator
  }
  
  GRPSUM <- function(omega, alpha, beta, gamma, eta, delta, sige, X, y, G, f1, f2, f3, f4, cases.only) {
    grp1 <- which(y >= delta & G == 1)
    grp2 <- which(y >= delta & G == 0)
    
    cases.only.sum <- sum(
      sum(f1(omega, alpha, beta, gamma, eta, delta, sige, X[grp1], y[grp1])),
      sum(f2(omega, alpha, beta, gamma, eta, delta, sige, X[grp2], y[grp2]))
    )
    
    if (cases.only) {
      return(cases.only.sum)
    }
    
    grp3 <- which(y < 0 & G == 1)
    grp4 <- which(y < 0 & G == 0)
    
    sum(
      cases.only.sum,
      sum(f3(omega, alpha, beta, gamma, eta, delta, sige, X[grp3], y[grp3])),
      sum(f4(omega, alpha, beta, gamma, eta, delta, sige, X[grp4], y[grp4]))
    )
  }
  
  DGamma <- function(omega, alpha, beta, gamma, eta, delta, sige, X, y, G, cases.only) {
    GRPSUM(omega, alpha, beta, gamma, eta, delta, sige, X, y, G, DR1Gamma, DR2Gamma, DR3Gamma, DR4Gamma, cases.only)
  }
  
  DEta <- function(omega, alpha, beta, gamma, eta, delta, sige, X, y, G, cases.only) {
    GRPSUM(omega, alpha, beta, gamma, eta, delta, sige, X, y, G, DR1Eta, DR2Eta, DR3Eta, DR4Eta, cases.only)
  }
  
  L <- function(params, omega, alpha, beta, delta, sige, X, y, G, cases.only) {
    gamma <- params[1]
    eta <- params[2]
    
    LL(omega, alpha, beta, gamma, eta, delta, sige, X, y, G, cases.only)
  }
  
  D <- function(params, omega, alpha, beta, delta, sige, X, y, G, cases.only) {
    gamma <- params[1]
    eta <- params[2]
    c(
      # cases only cumulative derivative wrt gamma
      DGamma(omega, alpha, beta, gamma, eta, delta, sige, X, y, G, cases.only),
      # cases only cumulative derivative wrt eta
      DEta(omega, alpha, beta, gamma, eta, delta, sige, X, y, G, cases.only)
    ) 
  }
  
  LL <- function(omega, alpha, beta, gamma, eta, delta, sige, X, y, G, cases.only) {
    GRPSUM(omega, alpha, beta, gamma, eta, delta, sige, X, y, G, lR1, lR2, lR3, lR4, cases.only)
  }
  
  list(
    sim = sim,
    L = L,
    D = D,
    LL = LL
  )
}

lrt.liabilityG.equations <- define.liabilityG.equations()
rm(define.liabilityG.equations)
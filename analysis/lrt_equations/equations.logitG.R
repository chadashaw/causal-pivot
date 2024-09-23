define.logitG.equations <- function() {
  sim <- function(
    n,
    alpha,
    beta,
    gamma,
    eta,
    omega
  ) {
    G <- rbinom(n = n, size = 1, prob = omega)
    
    X <- rnorm(n = n, mean = 0, sd = 1)
    
    GX <- G * X
    
    pY <- alpha + beta * X + gamma * G + eta * GX
    
    Y <- rbinom(n = n, size = 1, prob = expit(pY))
    
    list(
      Y = Y,
      X = X,
      G = G,
      GX = GX
    )
  }
  
  fY1XG1 <- function(alpha, beta, gamma, eta, X) {
    1 / (1 + exp(-1 * (alpha + beta * X + gamma + eta * X)))
  }
  
  fY1XG0 <- function(alpha, beta, X) {
    1 / (1 + exp(-1 * (alpha + beta * X)))
  }
  
  fY0XG1 <- function(alpha, beta, gamma, eta, X) {
    1 - fY1XG1(alpha, beta, gamma, eta, X)
  }
  
  fY0XG0 <- function(alpha, beta, X) {
    1 - fY1XG0(alpha, beta, X)
  }
  
  denomY1 <- function(omega, alpha, beta, gamma, eta, X) {
    y1g1 <- fY1XG1(alpha, beta, gamma, eta, X)
    y1g0 <- fY1XG0(alpha, beta, X)
    omega * y1g1 + (1 - omega) * y1g0
  }
  
  denomY0 <- function(omega, alpha, beta, gamma, eta, X) {
    y0g1 <- fY0XG1(alpha, beta, gamma, eta, X)
    y0g0 <- fY0XG0(alpha, beta, X)
    omega * y0g1 + (1 - omega) * y0g0
  }
  
  R1 <- function(omega, alpha, beta, gamma, eta, X) {
    fY1XG1(alpha, beta, gamma, eta, X) /
      denomY1(omega, alpha, beta, gamma, eta, X)
  }
  
  R2 <- function(omega, alpha, beta, gamma, eta, X) {
    fY1XG0(alpha, beta, X) /
      denomY1(omega, alpha, beta, gamma, eta, X)
  }
  
  R3 <- function(omega, alpha, beta, gamma, eta, X) {
    fY0XG1(alpha, beta, gamma, eta, X) /
      denomY0(omega, alpha, beta, gamma, eta, X)
  }
  
  R4 <- function(omega, alpha, beta, gamma, eta, X) {
    fY0XG0(alpha, beta, X) /
      denomY0(omega, alpha, beta, gamma, eta, X)
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
  
  DR1Gamma <- function(omega, alpha, beta, gamma, eta, X) {
    numerator <- -1 + omega
    denominator <- 1 + exp(alpha + gamma + X * (beta + eta)) - omega + exp(gamma + X * eta) * omega
    - numerator / denominator
  }
  
  DR1Eta <- function(omega, alpha, beta, gamma, eta, X) {
    numerator <- X * (-1 + omega)
    denominator <- 1 + exp(alpha + gamma + X * (beta + eta)) - omega + exp(gamma + X * eta) * omega
    - numerator / denominator
  }
  
  DR2Gamma <- function(omega, alpha, beta, gamma, eta, X) {
    numerator <- exp(gamma + X * eta) * (1 + exp(alpha + X * beta)) * omega
    denominator <- (1 + exp(alpha + gamma + X * (beta + eta))) * (1 + exp(alpha + gamma + X * (beta + eta)) - omega + exp(gamma + X * eta) * omega)
    - numerator / denominator
  }
  
  DR2Eta <- function(omega, alpha, beta, gamma, eta, X) {
    numerator <- exp(gamma + X * eta) * (1 + exp(alpha + X * beta)) * X * omega
    denominator <- (1 + exp(alpha + gamma + X * (beta + eta))) * (1 + exp(alpha + gamma + X * (beta + eta)) - omega + exp(gamma + X * eta) * omega)
    - numerator / denominator
  }
  
  DR3Gamma <- function(omega, alpha, beta, gamma, eta, X) {
    numerator <- exp(alpha + gamma + X * (beta + eta)) * (-1 + omega)
    denominator <- -1 + exp(alpha + gamma + X * (beta + eta)) * (-1 + omega) - exp(alpha + X * beta) * omega
    - numerator / denominator
  }
  
  DR3Eta <- function(omega, alpha, beta, gamma, eta, X) {
    numerator <- exp(alpha + gamma + X * (beta + eta)) * X * (-1 + omega)
    denominator <- -1 + exp(alpha + gamma + X * (beta + eta)) * (-1 + omega) - exp(alpha + X * beta) * omega
    - numerator / denominator
  }
  
  DR4Gamma <- function(omega, alpha, beta, gamma, eta, X) {
    numerator <- exp(alpha + gamma + X * (beta + eta)) * (1 + exp(alpha + X * beta)) * omega
    denominator <- (1 + exp(alpha + gamma + X * (beta + eta))) * 
      (-1 + exp(alpha + gamma + X * (beta + eta)) * (-1 + omega) - exp(alpha + X * beta) * omega)
    - numerator / denominator
  }
  
  DR4Eta <- function(omega, alpha, beta, gamma, eta, X) {
    numerator <- exp(alpha + gamma + X * (beta + eta)) * (1 + exp(alpha + X * beta)) * X * omega
    denominator <- (1 + exp(alpha + gamma + X * (beta + eta))) * 
      (-1 + exp(alpha + gamma + X * (beta + eta)) * (-1 + omega) - exp(alpha + X * beta) * omega)
    - numerator / denominator
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
  
  DGamma <- function(omega, alpha, beta, gamma, eta, X, Y, G, cases.only) {
    GRPSUM(omega, alpha, beta, gamma, eta, X, Y, G, DR1Gamma, DR2Gamma, DR3Gamma, DR4Gamma, cases.only)
  }
  
  DEta <- function(omega, alpha, beta, gamma, eta, X, Y, G, cases.only) {
    GRPSUM(omega, alpha, beta, gamma, eta, X, Y, G, DR1Eta, DR2Eta, DR3Eta, DR4Eta, cases.only)
  }
  
  LL <- function(omega, alpha, beta, gamma, eta, X, Y, G, cases.only) {
    GRPSUM(omega, alpha, beta, gamma, eta, X, Y, G, lR1, lR2, lR3, lR4, cases.only)
  }
  
  L <- function(params, omega, alpha, beta, X, Y, G, cases.only) {
    gamma <- params[1]
    eta <- params[2]
    
    LL(omega, alpha, beta, gamma, eta, X, Y, G, cases.only)
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
  
  list(
    sim = sim,
    D = D,
    L = L,
    DGamma = DGamma,
    DEta = DEta,
    LL = LL
  )
}

lrt.logitG.equations <- define.logitG.equations()
rm(define.logitG.equations)
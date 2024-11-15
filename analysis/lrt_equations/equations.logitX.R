define.logitX.equations <- function() {
  
  DR1Gamma <- function(alpha, beta, gamma, eta, X) {
    # Precompute reusable terms for clarity
    term1 <- exp(alpha + gamma)
    term2 <- exp(2 * (alpha + gamma))
    term3 <- exp(alpha + gamma + X * (beta + eta))
    term4 <- exp(X * (beta + eta))
    term5 <- exp(2 * alpha + 2 * gamma + X * (beta + eta))
    # Numerator
    numerator <- -(
      term1 * (
        -2 * term2 +
          2 * term1 * (-2 + beta^2 + 2 * beta * eta + eta^2) -
          4 * term3 * (-1 + beta^2 + 2 * beta * eta + eta^2) +
          term4 * (2 + beta^2 + 2 * beta * eta + eta^2) +
          term5 * (2 + beta^2 + 2 * beta * eta + eta^2) -
          2 * (1 + 2 * beta^2 + 4 * beta * eta + 2 * eta^2)
      )
    )
    # Denominator
    denominator <- (
      (1 + term1) *
        (1 + term3) *
        (2 + 2 * term2 + beta^2 + 2 * beta * eta + eta^2 -
           term1 * (-4 + beta^2 + 2 * beta * eta + eta^2))
    )
    # Result
    result <- numerator / denominator
    return(result)
  }
  
  DR1Eta <- function(alpha, beta, gamma, eta, X) {
    numerator.1 <- X
    
    denominator.1 <- 1 + exp(alpha + gamma + X * (beta + eta))
    
    numerator.2 <- 2 * (-1 + exp(alpha + gamma)) * (beta + eta)
    
    denominator.2 <- 2 +
      2 * exp(2 * (alpha + gamma)) +
      beta^2 + 2 * beta * eta + eta^2 -
      exp(alpha + gamma) * (-4 + beta^2 + 2 * beta * eta + eta^2)
    
    numerator.1 / denominator.1 + numerator.2 / denominator.2
  }
  
  DR2Gamma <- function(alpha, beta, gamma, eta, X) { 0 }
  DR2Eta <- function(alpha, beta, gamma, eta, X) { 0 }
  
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
    DR1Gamma = DR1Gamma,
    DR1Eta = DR1Eta,
    DR2Gamma = DR2Gamma,
    DR2Eta = DR2Eta
    # fisher.info = fisher.info
  )
}

lrt.logitX.equations <- define.logitX.equations()
rm(define.logitX.equations)
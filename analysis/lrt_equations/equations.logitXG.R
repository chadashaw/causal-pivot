define.equations.logitXG <- function() {
  # my.model <- function(alpha, beta, gamma, eta, X, G) {
  #   alpha + beta * X + gamma * G + eta * X * G
  # }
  # 
  # my.logit <- function(alpha, beta, gamma, eta, X, G) {
  #   e <- my.model(alpha, beta, gamma, eta, X, G)
  #   expit(e / (1 - e))
  # }
  
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
  
  R1 <- function(omega, alpha, beta, gamma, eta, X) {
    exp_alpha <- exp(alpha)
    exp_minus_alpha <- exp(-alpha)
    exp_alpha_gamma <- exp(alpha + gamma)
    exp_minus_alpha_gamma <- exp(-alpha - gamma)
    exp_minus_alpha_X_beta_eta <- exp(-alpha - X * beta - gamma - X * eta)
    
    term1_inner <- (2 * beta^2) / (exp_alpha^2 * (1 + exp_minus_alpha)^3) - beta^2 / (exp_alpha * (1 + exp_minus_alpha)^2)
    term1 <- (1 - omega) / (1 + exp(-alpha - (beta * term1_inner) / 2))
    
    term2_inner1 <- (2 * exp(-2 * alpha - 2 * gamma) * (-beta - eta)^2) / (1 + exp_minus_alpha_gamma)^3
    term2_inner2 <- (exp_minus_alpha_gamma * (-beta - eta)^2) / (1 + exp_minus_alpha_gamma)^2
    term2_inner <- term2_inner1 - term2_inner2
    term2_eta_correction <- (term2_inner * eta) / 2
    term2 <- omega / (1 + exp(-alpha - gamma - (beta * term2_inner) / 2 - term2_eta_correction))
    
    result <- 1 / ((1 + exp_minus_alpha_X_beta_eta) * (term1 + term2))
    
    return(result)
  }
  
  R2 <- function(omega, alpha, beta, gamma, eta, X) {
    exp_alpha <- exp(alpha)
    exp_minus_alpha <- exp(-alpha)
    exp_minus_alpha_gamma <- exp(-alpha - gamma)
    exp_minus_alpha_X_beta <- exp(-alpha - X * beta)
    
    term1_inner <- (2 * beta^2) / (exp_alpha^2 * (1 + exp_minus_alpha)^3) - beta^2 / (exp_alpha * (1 + exp_minus_alpha)^2)
    term1 <- (1 - omega) / (1 + exp(-alpha - (beta * term1_inner) / 2))
    
    term2_inner1 <- (2 * exp(-2 * alpha - 2 * gamma) * (-beta - eta)^2) / (1 + exp_minus_alpha_gamma)^3
    term2_inner2 <- (exp_minus_alpha_gamma * (-beta - eta)^2) / (1 + exp_minus_alpha_gamma)^2
    term2_inner <- term2_inner1 - term2_inner2
    term2_eta_correction <- (term2_inner * eta) / 2
    term2 <- omega / (1 + exp(-alpha - gamma - (beta * term2_inner) / 2 - term2_eta_correction))
    
    result <- 1 / ((1 + exp_minus_alpha_X_beta) * (term1 + term2))
    
    return(result)
  }
  
  R3 <- function(omega, alpha, beta, gamma, eta, X) {
    exp_alpha <- exp(alpha)
    exp_minus_alpha <- exp(-alpha)
    exp_minus_alpha_gamma <- exp(-alpha - gamma)
    exp_minus_alpha_X_beta_eta <- exp(-alpha - X * beta - gamma - X * eta)
    
    inner_term1 <- (-2 * beta^2) / (exp_alpha^2 * (1 + exp_minus_alpha)^3) + beta^2 / (exp_alpha * (1 + exp_minus_alpha)^2)
    term1 <- 1 - 1 / (1 + exp(-alpha - (beta * inner_term1) / 2))
    
    inner_term2_1 <- (-2 * exp(-2 * alpha - 2 * gamma) * (-beta - eta)^2) / (1 + exp_minus_alpha_gamma)^3
    inner_term2_2 <- (exp_minus_alpha_gamma * (-beta - eta)^2) / (1 + exp_minus_alpha_gamma)^2
    inner_term2 <- inner_term2_1 + inner_term2_2
    term2_eta_correction <- (inner_term2 * eta) / 2
    term2 <- 1 - 1 / (1 + exp(-alpha - gamma - (beta * inner_term2) / 2 - term2_eta_correction))
    
    numerator <- 1 - 1 / (1 + exp_minus_alpha_X_beta_eta)
    denominator <- (term1 * (1 - omega)) + (term2 * omega)
    
    result <- numerator / denominator
    
    return(result)
  }
  
  R4 <- function(omega, alpha, beta, gamma, eta, X) {
    exp_alpha <- exp(alpha)
    exp_minus_alpha <- exp(-alpha)
    exp_minus_alpha_gamma <- exp(-alpha - gamma)
    exp_minus_alpha_X_beta <- exp(-alpha - X * beta)
    
    inner_term1 <- (-2 * beta^2) / (exp_alpha^2 * (1 + exp_minus_alpha)^3) + beta^2 / (exp_alpha * (1 + exp_minus_alpha)^2)
    term1 <- 1 - 1 / (1 + exp(-alpha - (beta * inner_term1) / 2))
    
    inner_term2_1 <- (-2 * exp(-2 * alpha - 2 * gamma) * (-beta - eta)^2) / (1 + exp_minus_alpha_gamma)^3
    inner_term2_2 <- (exp_minus_alpha_gamma * (-beta - eta)^2) / (1 + exp_minus_alpha_gamma)^2
    inner_term2 <- inner_term2_1 + inner_term2_2
    term2_eta_correction <- (inner_term2 * eta) / 2
    term2 <- 1 - 1 / (1 + exp(-alpha - gamma - (beta * inner_term2) / 2 - term2_eta_correction))
    
    numerator <- 1 - 1 / (1 + exp_minus_alpha_X_beta)
    denominator <- (term1 * (1 - omega)) + (term2 * omega)
    
    result <- numerator / denominator
    
    return(result)
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
    exp_alpha <- exp(alpha)
    exp_gamma <- exp(gamma)
    exp_alpha_gamma <- exp(alpha + gamma)
    exp_minus_alpha <- exp(-alpha)
    exp_minus_alpha_gamma <- exp(-alpha - gamma)
    
    ### Part 1 ###
    part1_term1 <- exp_alpha * (-1 + omega)
    part1_term2 <- exp((exp_alpha * (-1 + exp_alpha) * beta^3) / (2 * (1 + exp_alpha)^3))
    part1_inner1 <- part1_term1 / (exp_alpha + part1_term2)
    
    part1_inner2_term1 <- 2 * (1 + exp_alpha_gamma)^3 * alpha + 2 * gamma + 2 * exp(3 * (alpha + gamma)) * gamma
    part1_inner2_term2 <- exp(2 * (alpha + gamma)) * (beta^3 - 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)
    part1_inner2_term3 <- exp_alpha_gamma * (beta^3 + 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)
    part1_inner2 <- part1_inner2_term1 - part1_inner2_term2 + part1_inner2_term3
    part1_inner2 <- omega / (1 + exp(-(part1_inner2 / (2 * (1 + exp_alpha_gamma)^3))))
    
    part1 <- exp(-alpha - gamma) * (part1_inner1 + part1_inner2)
    
    ### Part 2 ###
    part2_term1 <- 2 * (1 + exp_alpha_gamma)^3 * alpha + exp_alpha_gamma * beta^3 - exp(2 * (alpha + gamma)) * beta^3
    part2_term2 <- 2 * gamma + 6 * exp_alpha_gamma * gamma + 6 * exp(2 * (alpha + gamma)) * gamma + 2 * exp(3 * (alpha + gamma)) * gamma
    part2_term3 <- 3 * exp_alpha_gamma * beta^2 * eta - 3 * exp(2 * (alpha + gamma)) * beta^2 * eta
    part2_term4 <- 3 * exp_alpha_gamma * beta * eta^2 - 3 * exp(2 * (alpha + gamma)) * beta * eta^2
    part2_term5 <- exp_alpha_gamma * eta^3 - exp(2 * (alpha + gamma)) * eta^3
    part2_term6 <- -2 * (1 + exp_alpha_gamma)^3 * X * (beta + eta)
    part2_inner <- (part2_term1 + part2_term2 + part2_term3 + part2_term4 + part2_term5 + part2_term6) / (2 * (1 + exp_alpha_gamma)^3)
    part2_exp_inner <- exp(part2_inner)
    
    part2 <- -0.5 * part2_exp_inner * (1 + exp(alpha + gamma + X * (beta + eta))) * 
      (2 + 2 * exp(4 * (alpha + gamma)) - 4 * exp(2 * (alpha + gamma)) * (-3 + beta^3 + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3) + 
         exp_alpha_gamma * (8 + beta^3 + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3) + 
         exp(3 * (alpha + gamma)) * (8 + beta^3 + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)) * omega
    
    ### Part 3 ###
    part3_inner1 <- exp_alpha * (-1 + omega)
    part3_inner2 <- exp((exp_alpha * (-1 + exp_alpha) * beta^3) / (2 * (1 + exp_alpha)^3))
    part3_term1 <- part3_inner1 / (exp_alpha + part3_inner2)
    
    part3_term2_inner1 <- 2 * (1 + exp_alpha_gamma)^3 * alpha + 2 * gamma + 2 * exp(3 * (alpha + gamma)) * gamma
    part3_term2_inner2 <- exp(2 * (alpha + gamma)) * (beta^3 - 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)
    part3_term2_inner3 <- exp_alpha_gamma * (beta^3 + 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)
    part3_term2_inner <- part3_term2_inner1 - part3_term2_inner2 + part3_term2_inner3
    part3_term2 <- omega / (1 + exp(-(part3_term2_inner / (2 * (1 + exp_alpha_gamma)^3))))
    
    part3 <- (part3_term1 - part3_term2) / exp(X * (beta + eta))
    
    ### Combined Numerator ###
    numerator <- part1 * (part2 + part3)
    
    ### Part 4 (Denominator) ###
    denom_term1_inner1 <- exp_alpha * (-1 + omega)
    denom_term1_inner2 <- exp((exp_alpha * (-1 + exp_alpha) * beta^3) / (2 * (1 + exp_alpha)^3))
    denom_term1 <- denom_term1_inner1 / (exp_alpha + denom_term1_inner2)
    
    denom_term2_inner1 <- 2 * (1 + exp_alpha_gamma)^3 * alpha + 2 * gamma + 2 * exp(3 * (alpha + gamma)) * gamma
    denom_term2_inner2 <- exp(2 * (alpha + gamma)) * (beta^3 - 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)
    denom_term2_inner3 <- exp_alpha_gamma * (beta^3 + 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)
    denom_term2_inner <- denom_term2_inner1 - denom_term2_inner2 + denom_term2_inner3
    denom_term2 <- omega / (1 + exp(-(denom_term2_inner / (2 * (1 + exp_alpha_gamma)^3))))
    
    denominator <- (1 + exp(-alpha - gamma - X * (beta + eta))) * (denom_term1 - denom_term2)^2
    
    ### Result ###
    result <- numerator / denominator
    return(result)
  }

  DR1Eta <- function(omega, alpha, beta, gamma, eta, X) {
    exp_alpha <- exp(alpha)
    exp_gamma <- exp(gamma)
    exp_alpha_gamma <- exp(alpha + gamma)
    exp_minus_alpha <- exp(-alpha)
    exp_minus_alpha_gamma <- exp(-alpha - gamma)
    
    ### Part 1 ###
    part1_term1 <- exp_alpha * (-1 + omega)
    part1_term2 <- exp((exp_alpha * (-1 + exp_alpha) * beta^3) / (2 * (1 + exp_alpha)^3))
    part1_inner1 <- part1_term1 / (exp_alpha + part1_term2)
    
    part1_inner2_term1 <- 2 * (1 + exp_alpha_gamma)^3 * alpha + 2 * gamma + 2 * exp(3 * (alpha + gamma)) * gamma
    part1_inner2_term2 <- exp(2 * (alpha + gamma)) * (beta^3 - 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)
    part1_inner2_term3 <- exp_alpha_gamma * (beta^3 + 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)
    part1_inner2 <- part1_inner2_term1 - part1_inner2_term2 + part1_inner2_term3
    part1_inner2 <- omega / (1 + exp(-(part1_inner2 / (2 * (1 + exp_alpha_gamma)^3))))
    
    part1 <- exp(-alpha - gamma) * (part1_inner1 + part1_inner2)
    
    ### Part 2 ###
    part2_term1 <- 4 * (1 + exp_alpha_gamma)^3 * alpha + exp_alpha_gamma * beta^3 + exp(2 * (alpha + gamma)) * beta^3
    part2_term2 <- 4 * gamma + 12 * exp_alpha_gamma * gamma + 4 * exp(3 * (alpha + gamma)) * gamma
    part2_term3 <- 3 * exp_alpha_gamma * beta^2 * eta + 3 * exp(2 * (alpha + gamma)) * beta^2 * eta
    part2_term4 <- 3 * exp_alpha_gamma * beta * eta^2 + 3 * exp(2 * (alpha + gamma)) * beta * eta^2
    part2_term5 <- exp_alpha_gamma * eta^3 + exp(2 * (alpha + gamma)) * eta^3
    part2_term6 <- -2 * (1 + exp_alpha_gamma)^3 * X * (beta + eta)
    part2_inner <- (part2_term1 + part2_term2 + part2_term3 + part2_term4 + part2_term5 + part2_term6) / (2 * (1 + exp_alpha_gamma)^3)
    part2_exp_inner <- exp(part2_inner)
    
    part2 <- part2_exp_inner * (-1 + exp(alpha + gamma)) * (1 + exp(alpha + gamma + X * (beta + eta))) * 
      (beta + eta)^2 * omega / (2 * (1 + exp_alpha_gamma)^3 * 
                                  (exp((exp(2 * (alpha + gamma)) * (beta^3 - 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)) / (2 * (1 + exp_alpha_gamma)^3)) + 
                                     exp((2 * (1 + exp_alpha_gamma)^3 * alpha + 2 * gamma + 2 * exp(3 * (alpha + gamma)) * gamma + exp_alpha_gamma * (beta^3 + 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)) / (2 * (1 + exp_alpha_gamma)^3)))^2)
    
    ### Part 3 ###
    part3_inner1 <- exp_alpha * (-1 + omega)
    part3_inner2 <- exp((exp_alpha * (-1 + exp_alpha) * beta^3) / (2 * (1 + exp_alpha)^3))
    part3_term1 <- part3_inner1 / (exp_alpha + part3_inner2)
    
    part3_term2_inner1 <- 2 * (1 + exp_alpha_gamma)^3 * alpha + 2 * gamma + 2 * exp(3 * (alpha + gamma)) * gamma
    part3_term2_inner2 <- exp(2 * (alpha + gamma)) * (beta^3 - 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)
    part3_term2_inner3 <- exp_alpha_gamma * (beta^3 + 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)
    part3_term2_inner <- part3_term2_inner1 - part3_term2_inner2 + part3_term2_inner3
    part3_term2 <- omega / (1 + exp(-(part3_term2_inner / (2 * (1 + exp_alpha_gamma)^3))))
    
    part3 <- (X * (part3_term1 - part3_term2)) / exp(X * (beta + eta))
    
    ### Combined Numerator ###
    numerator <- part1 * (part2 + part3)
    
    ### Part 4 (Denominator) ###
    denom_term1_inner1 <- exp_alpha * (-1 + omega)
    denom_term1_inner2 <- exp((exp_alpha * (-1 + exp_alpha) * beta^3) / (2 * (1 + exp_alpha)^3))
    denom_term1 <- denom_term1_inner1 / (exp_alpha + denom_term1_inner2)
    
    denom_term2_inner1 <- 2 * (1 + exp_alpha_gamma)^3 * alpha + 2 * gamma + 2 * exp(3 * (alpha + gamma)) * gamma
    denom_term2_inner2 <- exp(2 * (alpha + gamma)) * (beta^3 - 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)
    denom_term2_inner3 <- exp_alpha_gamma * (beta^3 + 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)
    denom_term2_inner <- denom_term2_inner1 - denom_term2_inner2 + denom_term2_inner3
    denom_term2 <- omega / (1 + exp(-(denom_term2_inner / (2 * (1 + exp_alpha_gamma)^3))))
    
    denominator <- (1 + exp(-alpha - gamma - X * (beta + eta))) * (denom_term1 - denom_term2)^2
    
    ### Result ###
    result <- numerator / denominator
    return(result)
  }
  
  DR2Gamma <- function(omega, alpha, beta, gamma, eta, X) {
    exp_alpha <- exp(alpha)
    exp_gamma <- exp(gamma)
    exp_alpha_gamma <- exp(alpha + gamma)
    
    ### Part 1 ###
    part1_inner <- (exp_alpha * beta^3 + 2 * gamma + 6 * exp(2 * alpha) * gamma + 2 * exp(3 * alpha) * gamma) / (2 * (1 + exp_alpha)^3)
    part1 <- -0.5 * exp(part1_inner)
    
    ### Part 2 ###
    part2_inner <- (exp_alpha * (-1 + exp_alpha) * beta^3) / (2 * (1 + exp_alpha)^3)
    part2 <- exp_alpha + exp(part2_inner)
    
    ### Part 3 ###
    part3_term1 <- 2 + 2 * exp(4 * (alpha + gamma))
    part3_term2 <- -4 * exp(2 * (alpha + gamma)) * (-3 + beta^3 + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)
    part3_term3 <- exp_alpha_gamma * (8 + beta^3 + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)
    part3_term4 <- exp(3 * (alpha + gamma)) * (8 + beta^3 + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)
    part3 <- (part3_term1 + part3_term2 + part3_term3 + part3_term4) * omega
    
    ### Combined Numerator ###
    numerator <- part1 * part2 * part3
    
    ### Part 4 ###
    part4_inner <- (2 * (1 + exp_alpha_gamma)^3 * alpha + 2 * gamma + 2 * exp(3 * (alpha + gamma)) * gamma - exp(2 * (alpha + gamma)) * (beta^3 - 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3) + exp_alpha_gamma * (beta^3 + 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)) / (2 * (1 + exp_alpha_gamma)^3)
    part4 <- (1 + exp(alpha + gamma))^4 * (1 + exp(part4_inner))
    
    ### Part 5 ###
    part5_inner1 <- (exp_alpha * (beta^3 - 6 * gamma)) / (2 * (1 + exp_alpha)^3)
    part5_inner2 <- (exp_alpha * ((beta^3 - 6 * gamma) / (1 + exp_alpha)^3 + (exp(gamma) * (-1 + exp_alpha_gamma) * (beta + eta)^3) / (1 + exp_alpha_gamma)^3)) / 2
    part5_inner3 <- (2 * gamma + 2 * exp(3 * alpha) * gamma + exp(2 * alpha) * (beta^3 + 6 * gamma)) / (2 * (1 + exp_alpha)^3)
    part5 <- exp(alpha + part5_inner1 + gamma) - exp(part5_inner2) * (-1 + omega) + exp(part5_inner3) * omega
    
    ### Combined Denominator ###
    denominator <- part4 * part5
    
    ### Result ###
    result <- numerator / denominator
    return(result)
  }
  
  DR2Eta <- function(omega, alpha, beta, gamma, eta, X) {
    exp_alpha <- exp(alpha)
    exp_gamma <- exp(gamma)
    exp_alpha_gamma <- exp(alpha + gamma)
    
    ### Part 1 ###
    part1_term1 <- 2 * (1 + exp_alpha)^3 * alpha
    part1_term2 <- -exp_alpha * (beta^3 - 12 * gamma)
    part1_term3 <- 4 * gamma
    part1_term4 <- 4 * exp(3 * alpha) * gamma
    part1_term5 <- exp(2 * alpha) * (beta^3 + 12 * gamma)
    part1_inner <- (part1_term1 + part1_term2 + part1_term3 + part1_term4 + part1_term5) / (2 * (1 + exp_alpha)^3)
    part1_exp_inner <- exp(part1_inner)
    
    part1 <- 3 * part1_exp_inner * (1 + exp(alpha - (exp_alpha * (-1 + exp_alpha) * beta^3) / (2 * (1 + exp_alpha)^3))) * 
      (-1 + exp_alpha_gamma) * (beta + eta)^2 * omega
    
    ### Part 2 ###
    part2_term1_inner <- 2 * (1 + exp_alpha_gamma)^3 * alpha + 2 * gamma + 2 * exp(3 * (alpha + gamma)) * gamma
    part2_term1_inner <- part2_term1_inner - exp(2 * (alpha + gamma)) * (beta^3 - 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)
    part2_term1_inner <- part2_term1_inner + exp_alpha_gamma * (beta^3 + 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)
    part2_term1 <- exp(part2_term1_inner / (2 * (1 + exp_alpha_gamma)^3))
    
    part2 <- 2 * (1 + exp_alpha_gamma)^3 * (1 + part2_term1)
    
    ### Part 3 ###
    part3_term1 <- exp_alpha_gamma
    part3_term2 <- exp((exp_alpha_gamma * (-1 + exp_alpha_gamma) * (beta + eta)^3) / (2 * (1 + exp_alpha_gamma)^3)) * (-1 + omega)
    part3_term3 <- (-(exp_alpha * (beta^3 - 6 * gamma)) + 2 * gamma + 2 * exp(3 * alpha) * gamma + exp(2 * alpha) * (beta^3 + 6 * gamma)) / (2 * (1 + exp_alpha)^3)
    part3_term3 <- exp(part3_term3) * omega
    
    part3 <- part3_term1 - part3_term2 + part3_term3
    
    ### Combined Denominator ###
    denominator <- part2 * part3
    
    ### Result ###
    result <- part1 / denominator
    return(result)
  }
  
  DR3Gamma <- function(omega, alpha, beta, gamma, eta, X) {
    exp_alpha <- exp(alpha)
    exp_gamma <- exp(gamma)
    exp_alpha_gamma <- exp(alpha + gamma)
    exp_minus_alpha_gamma <- exp(-alpha - gamma)
    exp_minus_alpha <- exp(-alpha)
    exp_minus_alpha_gamma_X <- exp(-alpha - gamma - X * (beta + eta))
    exp_alpha_plus_gamma_plus_X <- exp(alpha + gamma + X * (beta + eta))
    
    ### Part 1 ###
    part1_inner1 <- (1 + exp(alpha + (exp_alpha * (-1 + exp_alpha) * beta^3) / (2 * (1 + exp_alpha)^3)))
    part1_inner2 <- (2 * (1 + exp_alpha_gamma)^3 * alpha + 2 * gamma + 2 * exp(3 * (alpha + gamma)) * gamma - exp_alpha_gamma * (beta^3 - 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3) + exp(2 * (alpha + gamma)) * (beta^3 + 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)) / (2 * (1 + exp_alpha_gamma)^3)
    part1 <- exp_minus_alpha_gamma * (1 + exp_alpha_plus_gamma_plus_X) * (-((-1 + omega) / part1_inner1) + omega / (1 + exp(part1_inner2)))
    
    ### Part 2 ###
    part2_inner1 <- (4 * (1 + exp_alpha_gamma)^3 * alpha + 4 * gamma + 4 * exp(3 * (alpha + gamma)) * gamma + exp_alpha_gamma * (beta + eta)^3 + exp(2 * (alpha + gamma)) * (beta^3 + 12 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)) / (2 * (1 + exp_alpha_gamma)^3)
    part2_inner2 <- 2 + 2 * exp(4 * (alpha + gamma)) - exp_alpha_gamma * (-8 + beta^3 + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3) - exp(3 * (alpha + gamma)) * (-8 + beta^3 + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3) + 4 * exp(2 * (alpha + gamma)) * (3 + beta^3 + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)
    part2 <- (exp(part2_inner1) * part2_inner2 * omega) / (2 * (1 + exp_alpha_gamma)^4 * (1 + exp_alpha_plus_gamma_plus_X))
    
    ### Part 3 ###
    part3_inner1 <- (2 * (1 + exp_alpha_gamma)^3 * alpha + 2 * gamma + 2 * exp(3 * (alpha + gamma)) * gamma - exp_alpha_gamma * (beta^3 - 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3) + exp(2 * (alpha + gamma)) * (beta^3 + 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)) / (2 * (1 + exp_alpha_gamma)^3)
    part3 <- (-((-1 + omega) / (1 + exp(alpha + (exp_alpha * (-1 + exp_alpha) * beta^3) / (2 * (1 + exp_alpha)^3)))) + omega / (1 + exp(part3_inner1))) / (exp(X * (beta + eta)) * (1 + exp_minus_alpha_gamma_X)^2)
    
    ### Combined Numerator ###
    numerator <- part1 * (part2 - part3)
    
    ### Part 4 (Denominator) ###
    part4_inner1 <- (exp_alpha * (-1 + exp_alpha) * beta^3) / (2 * (1 + exp_alpha)^3)
    part4_inner2 <- (2 * (1 + exp_alpha_gamma)^3 * alpha + 2 * gamma + 2 * exp(3 * (alpha + gamma)) * gamma - exp_alpha_gamma * (beta^3 - 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3) + exp(2 * (alpha + gamma)) * (beta^3 + 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)) / (2 * (1 + exp_alpha_gamma)^3)
    part4 <- ((-1 + omega) / (1 + exp(alpha + part4_inner1)) - omega / (1 + exp(part4_inner2)))^2
    
    ### Result ###
    result <- numerator / part4
    return(result)
  }
  
  DR3Eta <- function(omega, alpha, beta, gamma, eta, X) {
    exp_alpha <- exp(alpha)
    exp_gamma <- exp(gamma)
    exp_alpha_gamma <- exp(alpha + gamma)
    exp_minus_alpha_gamma <- exp(-alpha - gamma)
    exp_minus_alpha <- exp(-alpha)
    exp_minus_alpha_gamma_X <- exp(-alpha - gamma - X * (beta + eta))
    exp_alpha_plus_gamma_plus_X <- exp(alpha + gamma + X * (beta + eta))
    
    ### Part 1 ###
    part1_inner1 <- (1 + exp(alpha + (exp_alpha * (-1 + exp_alpha) * beta^3) / (2 * (1 + exp_alpha)^3)))
    part1_inner2 <- (2 * (1 + exp_alpha_gamma)^3 * alpha + 2 * gamma + 2 * exp(3 * (alpha + gamma)) * gamma - exp_alpha_gamma * (beta^3 - 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3) + exp(2 * (alpha + gamma)) * (beta^3 + 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)) / (2 * (1 + exp_alpha_gamma)^3)
    part1 <- exp_minus_alpha_gamma * (1 + exp_alpha_plus_gamma_plus_X) * (-((-1 + omega) / part1_inner1) + omega / (1 + exp(part1_inner2)))
    
    ### Part 2 ###
    part2_inner1 <- (6 * (1 + exp_alpha_gamma)^3 * alpha + 6 * gamma + 6 * exp(3 * (alpha + gamma)) * gamma + exp_alpha_gamma * (-beta^3 + 18 * gamma - 3 * beta^2 * eta - 3 * beta * eta^2 - eta^3) + exp(2 * (alpha + gamma)) * (beta^3 + 18 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)) / (2 * (1 + exp_alpha_gamma)^3)
    part2 <- ((3 * exp(part2_inner1) * (-1 + exp_alpha_gamma) * (beta + eta)^2 * omega) / 
                (2 * (1 + exp_alpha_gamma)^3 * (1 + exp_alpha_plus_gamma_plus_X) * (1 + exp(part1_inner2))^2))
    
    ### Part 3 ###
    part3_inner1 <- (2 * (1 + exp_alpha_gamma)^3 * alpha + 2 * gamma + 2 * exp(3 * (alpha + gamma)) * gamma - exp_alpha_gamma * (beta^3 - 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3) + exp(2 * (alpha + gamma)) * (beta^3 + 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)) / (2 * (1 + exp_alpha_gamma)^3)
    part3 <- (X * (-((-1 + omega) / (1 + exp(alpha + (exp_alpha * (-1 + exp_alpha) * beta^3) / (2 * (1 + exp_alpha)^3)))) + omega / (1 + exp(part3_inner1)))) / 
      (exp(X * (beta + eta)) * (1 + exp_minus_alpha_gamma_X)^2)
    
    ### Combined Numerator ###
    numerator <- part1 * (part2 - part3)
    
    ### Part 4 (Denominator) ###
    part4_inner1 <- (exp_alpha * (-1 + exp_alpha) * beta^3) / (2 * (1 + exp_alpha)^3)
    part4_inner2 <- (2 * (1 + exp_alpha_gamma)^3 * alpha + 2 * gamma + 2 * exp(3 * (alpha + gamma)) * gamma - exp_alpha_gamma * (beta^3 - 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3) + exp(2 * (alpha + gamma)) * (beta^3 + 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)) / (2 * (1 + exp_alpha_gamma)^3)
    part4 <- ((-1 + omega) / (1 + exp(alpha + part4_inner1)) - omega / (1 + exp(part4_inner2)))^2
    
    ### Result ###
    result <- numerator / part4
    return(result)
  }
  
  DR4Gamma <- function(omega, alpha, beta, gamma, eta, X) {
    exp_alpha <- exp(alpha)
    exp_gamma <- exp(gamma)
    exp_alpha_gamma <- exp(alpha + gamma)
    
    ### Part 1 ###
    part1_inner <- (2 * (1 + exp_alpha_gamma)^3 * alpha + 2 * gamma + 
                      2 * exp(3 * (alpha + gamma)) * gamma + 
                      exp_alpha_gamma * (-beta^3 + 6 * gamma - 3 * beta^2 * eta - 3 * beta * eta^2 - eta^3) + 
                      exp(2 * (alpha + gamma)) * (beta^3 + 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)) / 
      (2 * (1 + exp_alpha_gamma)^3)
    part1 <- -0.5 * exp(part1_inner)
    
    ### Part 2 ###
    part2_inner <- (exp_alpha * (-1 + exp_alpha) * beta^3) / (2 * (1 + exp_alpha)^3)
    part2 <- 1 + exp(alpha + part2_inner)
    
    ### Part 3 ###
    part3 <- (2 + 2 * exp(4 * (alpha + gamma)) - 
                exp(alpha + gamma) * (-8 + beta^3 + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3) - 
                exp(3 * (alpha + gamma)) * (-8 + beta^3 + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3) + 
                4 * exp(2 * (alpha + gamma)) * (3 + beta^3 + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)) * omega
    
    ### Combined Numerator ###
    numerator <- part1 * part2 * part3
    
    ### Part 4 ###
    part4_inner <- (2 * (1 + exp_alpha_gamma)^3 * alpha + 2 * gamma + 
                      2 * exp(3 * (alpha + gamma)) * gamma - 
                      exp_alpha_gamma * (beta^3 - 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3) + 
                      exp(2 * (alpha + gamma)) * (beta^3 + 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)) / 
      (2 * (1 + exp_alpha_gamma)^3)
    part4 <- (1 + exp(part4_inner))
    
    ### Part 5 ###
    part5 <- (-1 + exp(part4_inner) * (-1 + omega) - 
                exp(alpha + part2_inner) * omega)
    
    ### Combined Denominator ###
    denominator <- part4 * part5
    
    ### Result ###
    result <- numerator / denominator
    return(result)
  }
  
  DR4Eta <- function(omega, alpha, beta, gamma, eta, X) {
    exp_alpha <- exp(alpha)
    exp_gamma <- exp(gamma)
    exp_alpha_gamma <- exp(alpha + gamma)
    
    ### Part 1 ###
    part1_inner <- (4 * (1 + exp_alpha_gamma)^3 * alpha + 4 * gamma + 
                      4 * exp(3 * (alpha + gamma)) * gamma + 
                      exp_alpha_gamma * (-beta^3 + 12 * gamma - 3 * beta^2 * eta - 3 * beta * eta^2 - eta^3) + 
                      exp(2 * (alpha + gamma)) * (beta^3 + 12 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)) / 
      (2 * (1 + exp_alpha_gamma)^3)
    part1 <- -3 * exp(part1_inner)
    
    ### Part 2 ###
    part2_inner <- (exp_alpha * (-1 + exp_alpha) * beta^3) / (2 * (1 + exp_alpha)^3)
    part2 <- 1 + exp(alpha + part2_inner)
    part2 <- part2 * (-1 + exp_alpha_gamma) * (beta + eta)^2 * omega
    
    ### Combined Numerator ###
    numerator <- part1 * part2
    
    ### Part 3 ###
    part3_inner <- (2 * (1 + exp_alpha_gamma)^3 * alpha + 2 * gamma + 
                      2 * exp(3 * (alpha + gamma)) * gamma - 
                      exp_alpha_gamma * (beta^3 - 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3) + 
                      exp(2 * (alpha + gamma)) * (beta^3 + 6 * gamma + 3 * beta^2 * eta + 3 * beta * eta^2 + eta^3)) / 
      (2 * (1 + exp_alpha_gamma)^3)
    part3 <- (1 + exp(part3_inner))
    
    ### Part 4 ###
    part4 <- (-1 + exp(part3_inner) * (-1 + omega) - 
                exp(alpha + part2_inner) * omega)
    
    ### Combined Denominator ###
    denominator <- 2 * (1 + exp_alpha_gamma)^3 * part3 * part4
    
    ### Result ###
    result <- numerator / denominator
    return(result)
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
  
  # fY1X <- function(alpha, beta, X) {
  #   return(1 / (1 + exp(-alpha - X * beta)))
  # }
  # 
  # fY0X <- function(alpha, beta, X) {
  #   return(1 - 1 / (1 + exp(-alpha - X * beta)))
  # }
  
  rXY1 <- function(alpha, beta, X) {
    exp_alpha <- exp(alpha)
    exp_minus_alpha <- exp(-alpha)
    exp_minus_alpha_X_beta <- exp(-alpha - X * beta)
    
    numerator <- 1 + exp(-alpha - beta / (2 * (1 + exp_minus_alpha_X_beta)))
    denominator <- 1 + exp_minus_alpha_X_beta
    
    result <- log(numerator / denominator)
    
    return(result)
  }
  
  rXY0 <- function(alpha, beta, X) {
    exp_alpha <- exp(alpha)
    exp_alpha_X_beta <- exp(alpha + X * beta)
    exp_minus_alpha_X_beta <- exp(-alpha - X * beta)
    
    term1 <- 1 + exp(alpha + beta / (2 + 2 * exp_alpha_X_beta))
    term2 <- 1 - 1 / (1 + exp_minus_alpha_X_beta)
    
    result <- log(term1 * term2)
    
    return(result)
  }
  
  LL <- function(omega, alpha, beta, gamma, eta, X, Y, G, cases.only = T) {
    alt.sum <- GRPSUM(omega, alpha, beta, gamma, eta, X, Y, G, lR1, lR2, lR3, lR4, cases.only)
    null.sum <- sum(rXY1(alpha, beta, X[which(Y == 1)])) + sum(ifelse(cases.only, 0, rXY0(alpha, beta, X[which(Y == 0)])))
    
    alt.sum - null.sum
  }
  
  list(
    DGamma = DGamma,
    DEta = DEta,
    LL = LL
  )
}

logitXG <- define.equations.logitXG()
rm(define.equations.logitXG)

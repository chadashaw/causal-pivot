raise.power <- function(base, exponent) {
  base ** exponent
}

mean.X1 <- function(alpha, beta, gamma, eta, X) {
  (-2*raise.power(exp(1),-alpha - X*beta - gamma - X*eta)*(-beta - eta))/
    (raise.power(1 + raise.power(exp(1),-alpha - X*beta - gamma - X*eta),2)*(1/(1 + raise.power(exp(1),-alpha - gamma)) + 
                                                                               0.5*((2*raise.power(exp(1),-2*alpha - 2*gamma)*raise.power(-beta - eta,2))/raise.power(1 + raise.power(exp(1),-alpha - gamma),3) - (raise.power(exp(1),-alpha - gamma)*raise.power(-beta - eta,2))/raise.power(1 + raise.power(exp(1),-alpha - gamma),2)))) + 
    (2*raise.power(exp(1),-2*alpha - 2*X*beta - 2*gamma - 2*X*eta)*X*raise.power(-beta - eta,2))/
    (raise.power(1 + raise.power(exp(1),-alpha - X*beta - gamma - X*eta),3)*(1/(1 + raise.power(exp(1),-alpha - gamma)) + 
                                                                               0.5*((2*raise.power(exp(1),-2*alpha - 2*gamma)*raise.power(-beta - eta,2))/raise.power(1 + raise.power(exp(1),-alpha - gamma),3) - (raise.power(exp(1),-alpha - gamma)*raise.power(-beta - eta,2))/raise.power(1 + raise.power(exp(1),-alpha - gamma),2)))) - 
    (raise.power(exp(1),-alpha - X*beta - gamma - X*eta)*X*raise.power(-beta - eta,2))/
    (raise.power(1 + raise.power(exp(1),-alpha - X*beta - gamma - X*eta),2)*(1/(1 + raise.power(exp(1),-alpha - gamma)) + 
                                                                               0.5*((2*raise.power(exp(1),-2*alpha - 2*gamma)*raise.power(-beta - eta,2))/raise.power(1 + raise.power(exp(1),-alpha - gamma),3) - (raise.power(exp(1),-alpha - gamma)*raise.power(-beta - eta,2))/raise.power(1 + raise.power(exp(1),-alpha - gamma),2))))
  
}

mean.X2 <- function(alpha, beta, gamma, eta, X) {
  (2*raise.power(exp(1),-alpha - X*beta)*beta)/(raise.power(1 + raise.power(exp(1),-alpha - X*beta),2)*(1/(1 + raise.power(exp(1),-alpha)) + 
                                                                                                          0.5*((2*raise.power(beta,2))/(raise.power(exp(1),2*alpha)*raise.power(1 + raise.power(exp(1),-alpha),3)) - raise.power(beta,2)/(raise.power(exp(1),alpha)*raise.power(1 + raise.power(exp(1),-alpha),2))))) + 
    (2*raise.power(exp(1),-2*alpha - 2*X*beta)*X*raise.power(beta,2))/
    (raise.power(1 + raise.power(exp(1),-alpha - X*beta),3)*(1/(1 + raise.power(exp(1),-alpha)) + 0.5*
                                                               ((2*raise.power(beta,2))/(raise.power(exp(1),2*alpha)*raise.power(1 + raise.power(exp(1),-alpha),3)) - raise.power(beta,2)/(raise.power(exp(1),alpha)*raise.power(1 + raise.power(exp(1),-alpha),2))))) - 
    (raise.power(exp(1),-alpha - X*beta)*X*raise.power(beta,2))/
    (raise.power(1 + raise.power(exp(1),-alpha - X*beta),2)*(1/(1 + raise.power(exp(1),-alpha)) + 0.5*
                                                               ((2*raise.power(beta,2))/(raise.power(exp(1),2*alpha)*raise.power(1 + raise.power(exp(1),-alpha),3)) - raise.power(beta,2)/(raise.power(exp(1),alpha)*raise.power(1 + raise.power(exp(1),-alpha),2)))))
  
}

mean.X3 <- function(alpha, beta, gamma, eta, X) {
  (2*raise.power(exp(1),-alpha - X*beta - gamma - X*eta)*(-beta - eta))/
    (raise.power(1 + raise.power(exp(1),-alpha - X*beta - gamma - X*eta),2)*(1 - 1/(1 + raise.power(exp(1),-alpha - gamma)) + 
                                                                               0.5*((-2*raise.power(exp(1),-2*alpha - 2*gamma)*raise.power(-beta - eta,2))/raise.power(1 + raise.power(exp(1),-alpha - gamma),3) + (raise.power(exp(1),-alpha - gamma)*raise.power(-beta - eta,2))/raise.power(1 + raise.power(exp(1),-alpha - gamma),2)))) - 
    (2*raise.power(exp(1),-2*alpha - 2*X*beta - 2*gamma - 2*X*eta)*X*raise.power(-beta - eta,2))/
    (raise.power(1 + raise.power(exp(1),-alpha - X*beta - gamma - X*eta),3)*(1 - 1/(1 + raise.power(exp(1),-alpha - gamma)) + 
                                                                               0.5*((-2*raise.power(exp(1),-2*alpha - 2*gamma)*raise.power(-beta - eta,2))/raise.power(1 + raise.power(exp(1),-alpha - gamma),3) + (raise.power(exp(1),-alpha - gamma)*raise.power(-beta - eta,2))/raise.power(1 + raise.power(exp(1),-alpha - gamma),2)))) + 
    (raise.power(exp(1),-alpha - X*beta - gamma - X*eta)*X*raise.power(-beta - eta,2))/
    (raise.power(1 + raise.power(exp(1),-alpha - X*beta - gamma - X*eta),2)*(1 - 1/(1 + raise.power(exp(1),-alpha - gamma)) + 
                                                                               0.5*((-2*raise.power(exp(1),-2*alpha - 2*gamma)*raise.power(-beta - eta,2))/raise.power(1 + raise.power(exp(1),-alpha - gamma),3) + (raise.power(exp(1),-alpha - gamma)*raise.power(-beta - eta,2))/raise.power(1 + raise.power(exp(1),-alpha - gamma),2))))
  
}

mean.X4 <- function(alpha, beta, gamma, eta, X) {
  (-2*raise.power(exp(1),-alpha - X*beta)*beta)/(raise.power(1 + raise.power(exp(1),-alpha - X*beta),2)*(1 - 1/(1 + raise.power(exp(1),-alpha - gamma)) + 
                                                                                                           0.5*((-2*raise.power(beta,2))/(raise.power(exp(1),2*alpha)*raise.power(1 + raise.power(exp(1),-alpha),3)) + raise.power(beta,2)/(raise.power(exp(1),alpha)*raise.power(1 + raise.power(exp(1),-alpha),2))))) - 
    (2*raise.power(exp(1),-2*alpha - 2*X*beta)*X*raise.power(beta,2))/
    (raise.power(1 + raise.power(exp(1),-alpha - X*beta),3)*(1 - 1/(1 + raise.power(exp(1),-alpha - gamma)) + 
                                                               0.5*((-2*raise.power(beta,2))/(raise.power(exp(1),2*alpha)*raise.power(1 + raise.power(exp(1),-alpha),3)) + raise.power(beta,2)/(raise.power(exp(1),alpha)*raise.power(1 + raise.power(exp(1),-alpha),2))))) + 
    (raise.power(exp(1),-alpha - X*beta)*X*raise.power(beta,2))/
    (raise.power(1 + raise.power(exp(1),-alpha - X*beta),2)*(1 - 1/(1 + raise.power(exp(1),-alpha - gamma)) + 
                                                               0.5*((-2*raise.power(beta,2))/(raise.power(exp(1),2*alpha)*raise.power(1 + raise.power(exp(1),-alpha),3)) + raise.power(beta,2)/(raise.power(exp(1),alpha)*raise.power(1 + raise.power(exp(1),-alpha),2)))))
  
}

params.eX <- list(
  alpha = -2.2,
  gamma = 1,
  eta = 0,
  X = 0
)

beta.range <- seq(0, 2, length.out = 1000)

test.colors.EX <- c(
  'E(X|Y=1,G=0)' = '#f42e3d',
  'E(X|Y=0,G=1)' = '#2ff481',
  'E(X|Y=1,G=1)' = '#e32ff4',
  'E(X|Y=0,G=0)' = '#2ff4e4'
)

(
  data.frame(
    beta = beta.range,
    eX.1 = f.call(mean.X1, modifyList(params.eX, list(beta = beta.range))),
    eX.2 = f.call(mean.X2, modifyList(params.eX, list(beta = beta.range))),
    eX.3 = f.call(mean.X3, modifyList(params.eX, list(beta = beta.range))),
    eX.4 = f.call(mean.X4, modifyList(params.eX, list(beta = beta.range)))
  ) %>%
    pivot_longer(cols = starts_with('eX'), names_to = 'grp', values_to = 'eX') %>%
    mutate(
      grp = case_when(
        grp == 'eX.1' ~ 'E(X|Y=1,G=1)',
        grp == 'eX.2' ~ 'E(X|Y=1,G=0)',
        grp == 'eX.3' ~ 'E(X|Y=0,G=1)',
        grp == 'eX.4' ~ 'E(X|Y=0,G=0)',
        .default = 'other'
      ),
      grp = factor(grp, levels = names(test.colors.EX)),
    ) %>%
    ggplot(aes(x = beta, y = eX, color = grp)) +
    geom_line(linewidth = 0.53) +
    scale_color_manual(values = test.colors.EX) +
    labs(
      x = expression(beta),
      y = 'E[X|Y,G]',
      color = 'Group'
    ) +
    theme_bw(base_family="Arial") +
    theme(
      panel.grid.major = element_line(color = "grey90", linewidth = 0.25),
      panel.grid.minor = element_line(color = "grey90", linewidth = 0.25),
      axis.text = element_text(size=8),
      axis.title = element_text(size=8, family = "Arial", face = "bold"),
      legend.text = element_text(size=6),
      legend.title = element_text(size=8),
    )
) %>% save.plot(
  'supp.fig2',
  root.dir = out.dir,
  w = 3.34,
  h = 3.34
)

mean.G1 <- function(omega, alpha, beta,gamma, eta, X) {
  omega/((1 + raise.power(exp(1),-alpha - X*beta - gamma - X*eta))*((1 - omega)/(1 + raise.power(exp(1),-alpha - X*beta)) + omega/(1 + raise.power(exp(1),-alpha - X*beta - gamma - X*eta))))
}

mean.G2 <- function(omega, alpha, beta,gamma, eta, X) {
  (1 - omega)/((1 + raise.power(exp(1),-alpha - X*beta))*((1 - omega)/(1 + raise.power(exp(1),-alpha - X*beta)) + omega/(1 + raise.power(exp(1),-alpha - X*beta - gamma - X*eta))))
}

mean.G3 <- function(omega, alpha, beta,gamma, eta, X) {
  ((1 - 1/(1 + raise.power(exp(1),-alpha - X*beta - gamma - X*eta)))*omega)/((1 - 1/(1 + raise.power(exp(1),-alpha - X*beta)))*(1 - omega) + (1 - 1/(1 + raise.power(exp(1),-alpha - X*beta - gamma - X*eta)))*omega)
}

mean.G4 <- function(omega, alpha, beta,gamma, eta, X) {
  ((1 - 1/(1 + raise.power(exp(1),-alpha - X*beta)))*(1 - omega))/((1 - 1/(1 + raise.power(exp(1),-alpha - X*beta)))*(1 - omega) + (1 - 1/(1 + raise.power(exp(1),-alpha - X*beta - gamma - X*eta)))*omega)
}

params.eG <- list(
  omega = 1e-3,
  alpha = -2.2,
  beta = 0.6,
  gamma = 1,
  eta = -0.4,
  X = 0
)

beta.range.eG <- seq(0, 2, length.out = 1000)

eG.Y1.Xminus1 <- f.call(mean.G1, modifyList(params.eG, list(X = -1, beta = beta.range.eG)))
eG.Y1.Xplus1 <- f.call(mean.G1, modifyList(params.eG, list(X = 1, beta = beta.range.eG)))
eG.Y0.Xminus1 <- f.call(mean.G3, modifyList(params.eG, list(X = -1, beta = beta.range.eG)))
eG.Y0.Xplus1 <- f.call(mean.G3, modifyList(params.eG, list(X = 1, beta = beta.range.eG)))

eG.labeller <- function(vals) {
  labels <- list(
    'cases' = bquote(.("RV") ^ .("+") * .(" Cases")),
    'controls' = bquote(.("RV") ^ .("+") * .(" Controls"))
  )
  
  sapply(vals, function(val) labels[[val]])
}

test.colors.eG <- c(
  'cases' = '#f42e3d',
  'controls' = '#2ff4e4'
)

(
  data.frame(
    beta = beta.range.eG,
    oG.1 = (eG.Y1.Xminus1 / (1 - eG.Y1.Xminus1)) / (eG.Y1.Xplus1 / (1 - eG.Y1.Xplus1)),
    oG.3 = (eG.Y0.Xminus1 / (1 - eG.Y0.Xminus1)) / (eG.Y0.Xplus1 / (1 - eG.Y0.Xplus1))
  ) %>%
    pivot_longer(cols = starts_with('oG'), names_to = 'grp', values_to = 'oG') %>%
    mutate(
      grp = case_when(
        grp == 'oG.1' ~ 'cases',
        grp == 'oG.3' ~ 'controls',
        .default = 'other'
      ),
      grp = factor(grp, levels = names(test.colors.eG)),
    ) %>%
    ggplot(aes(x = beta, y = oG, color = grp)) +
    geom_line() +
    scale_color_manual(values = test.colors.eG, labels = eG.labeller) +
    scale_y_continuous(lim = c(0, NA)) +
    labs(
      x = expression(bold(beta)),
      y = 'Odds Ratio',
      color = 'Group'
    ) +
    theme_bw(base_family="Arial") +
    theme(
      panel.grid.major = element_line(color = "grey90", linewidth = 0.25),
      panel.grid.minor = element_line(color = "grey90", linewidth = 0.25),
      axis.text = element_text(size=8),
      axis.title = element_text(size=8, family = "Arial", face = "bold"),
      legend.text = element_text(size=6),
      legend.title = element_text(size=8),
    )
) %>% save.plot(
  'supp.fig1',
  root.dir = out.dir,
  w = 3.34,
  h = 3.34
)


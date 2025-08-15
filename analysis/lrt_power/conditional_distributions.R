library(dplyr)
library(tidyr)
library(ggplot2)
source('utils.R')

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

p1 <- function(omega, alpha, beta, gamma, eta, X) {
  R1(omega, alpha, beta, gamma, eta, X) * dnorm(X, 0, 1)
}

p2 <- function(omega, alpha, beta, gamma, eta, X) {
  R2(omega, alpha, beta, gamma, eta, X) * dnorm(X, 0, 2)
}

p3 <- function(omega, alpha, beta, gamma, eta, X) {
  R3(omega, alpha, beta, gamma, eta, X) * dnorm(X, 0, 3)
}

p4 <- function(omega, alpha, beta, gamma, eta, X) {
  R4(omega, alpha, beta, gamma, eta, X) * dnorm(X, 0, 4)
}

params.pX <- list(
  omega = 0.001,
  alpha = 0,
  gamma = 1,
  beta = 0.6,
  eta = 0,
  X = 0
)

X.range <- seq(-4, 4, length.out = 1000)
beta.range <- seq(0, 2, length.out = 4)

test.colors.EX <- c(
  'p(X|Y=1,G=0)' = '#f42e3d',
  'p(X|Y=0,G=1)' = '#2ff481',
  'p(X|Y=1,G=1)' = '#e32ff4',
  'p(X|Y=0,G=0)' = '#2ff4e4'
)

# (
  lapply(beta.range, function(beta) {
    data.frame(
      beta = beta,
      X = X.range,
      pX.1 = f.call(p1, modifyList(params.pX, list(beta = beta, X = X.range))),
      pX.2 = f.call(p2, modifyList(params.pX, list(beta = beta, X = X.range))),
      pX.3 = f.call(p3, modifyList(params.pX, list(beta = beta, X = X.range))),
      pX.4 = f.call(p4, modifyList(params.pX, list(beta = beta, X = X.range)))
    )
  }) %>%
  do.call(bind_rows, .) %>%
    pivot_longer(cols = starts_with('pX'), names_to = 'grp', values_to = 'pX') %>%
    mutate(
      grp = case_when(
        grp == 'pX.1' ~ 'p(X|Y=1,G=1)',
        grp == 'pX.2' ~ 'p(X|Y=1,G=0)',
        grp == 'pX.3' ~ 'p(X|Y=0,G=1)',
        grp == 'pX.4' ~ 'p(X|Y=0,G=0)',
        .default = 'other'
      ),
      grp = factor(grp, levels = names(test.colors.EX)),
    ) %>%
    ggplot(aes(x = X, y = pX, color = grp)) +
    geom_line() +
    scale_color_manual(values = test.colors.EX) +
    facet_wrap(~beta) +
    labs(
      x = expression(beta),
      y = 'p[X|Y,G]',
      color = 'Group'
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(face = "bold", size = 12),
      axis.title = element_text(face = "bold", size = 12),
    )
# ) %>% save.plot('conditional.pX', root.dir = out.dir, w = 4.5, h = 4.5)

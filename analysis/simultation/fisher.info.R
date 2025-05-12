source('../lrt_equations/equations.logitG.R')
source('../lrt_equations/equations.logitX.R')

fisher.info <- function(omega, alpha, beta, gamma, eta, X, Y, G) {
  calc.I <- function(omega, alpha, beta, gamma, eta, X, dGGamma, dGEta, dXGamma, dXEta) {
    f.params <- list(
      omega = omega,
      alpha = alpha,
      beta = beta,
      gamma = gamma,
      eta = eta,
      X = X
    )
    
    dGg <- f.call(dGGamma, f.params)
    dGe <- f.call(dGEta, f.params)
    dXg <- f.call(dXGamma, f.params)
    dXe <- f.call(dXEta, f.params)
    
    i11 <- (dGg + dXg) ^ 2
    i12 <- i21 <- (dGg + dXg) * (dGe + dXe)
    i22 <- (dGe + dXe) ^ 2
    
    matrix(c(mean(i11), mean(i21), mean(i12), mean(i22)), nrow=2)
  }
  
  grp1 <- which(Y == 1 & G == 1)
  grp2 <- which(Y == 1 & G == 0)
  # grp3 <- which(Y == 0 & G == 1)
  # grp4 <- which(Y == 0 & G == 0)
  
  I1 <- calc.I(omega, alpha, beta, gamma, eta, X[grp1],
               lrt.logitG.equations$DR1Gamma,
               lrt.logitG.equations$DR1Eta,
               lrt.logitX.equations$DR1Gamma,
               lrt.logitX.equations$DR1Eta
               )
  I2 <- calc.I(omega, alpha, beta, gamma, eta, X[grp2],
               lrt.logitG.equations$DR2Gamma,
               lrt.logitG.equations$DR2Eta,
               lrt.logitX.equations$DR2Gamma,
               lrt.logitX.equations$DR2Eta
               )
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

# set params ####
params.test <- list(
  n = 5e5,
  omega = 1e-3,
  alpha = -2.2,
  beta = 0.6,
  gamma = 1.2,
  eta = -0.1
)

# simulate some data ####
sim.test <- f.call(lrt.logitG.equations$sim, params.test)

G.info <- f.call(lrt.logitG.equations$fisher.info, modifyList(params.test, sim.test))
G.x <- solve(G.info$I1 * G.info$n1 + G.info$I2 * G.info$n2)

all.info <- f.call(fisher.info, modifyList(params.test, sim.test))
all.x <- solve(all.info$I1 * all.info$n1 + all.info$I2 * all.info$n2)

sqrt(G.x[1,1])
sqrt(all.x[1,1])

sqrt(all.x[1,1]) / sqrt(G.x[1,1])
sqrt(all.x[2,2]) / sqrt(G.x[2,2])

sqrt(G.x[2,2])
sqrt(all.x[2,2])

det(G.x)
det(all.x)

det(G.x) / det(all.x)


source('../lrt_equations/equations.logitXG.R')
list2env(lrt.logitXG.equations, envir = environment())

params <- list(
  n = 5e5,
  omega = 0.001,
  alpha = -2.2,
  beta = 0.6,
  gamma = 1,
  eta = -0.4
)

sim.result <- f.call(sim, params)

G.info <- f.call(lrt.logitG.equations$fisher.info, modifyList(params, sim.result))
XG.info <- f.call(lrt.logitXG.equations$fisher.info, modifyList(params, sim.result))

G.x <- solve(G.info$I1 * G.info$n1 + G.info$I2 * G.info$n2)
XG.x <- solve(XG.info$I1 * XG.info$n1 + XG.info$I2 * XG.info$n2)

det(G.x)
det(XG.x)

G.x
XG.x

sqrt(G.x[1,1])
sqrt(XG.x[1,1])

sqrt(G.x[2,2])
sqrt(XG.x[2,2])

sim.df.1 <- as_tibble(f.call(sim, modifyList(params, list(omega = 1e-3)))) %>% mutate(omega = 1e-3)
sim.df.2 <- as_tibble(f.call(sim, modifyList(params, list(omega = 1e-2)))) %>% mutate(omega = 1e-2)

bind_rows(sim.df.1, sim.df.2) %>%
  mutate(omega = as.factor(omega)) %>%
  ggplot(aes(x = X, color = omega)) +
  geom_density() +
  facet_grid(rows = vars(Y), cols = vars(G))

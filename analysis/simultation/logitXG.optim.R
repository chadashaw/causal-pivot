do.mle <- function(params, sim.result, FUN, GRD) {
  cases.only <- T
  
  f.params <- modifyList(params, sim.result)
  f.params <- extract.args.for.function(FUN, f.params)
  f.params <- modifyList(f.params, list(cases.only = cases.only))
  
  start <- c(0, 0)
  
  maxit <- 10000
  
  optim.params <- list(
    par = start,
    fn = FUN,
    gr = GRD,
    # method = "SANN",
    method = "BFGS",
    control = list(
      maxit = maxit,
      fnscale = -1
    )
  )
  
  mle.result <- f.call(optim, c(f.params, optim.params), force.incl = names(f.params))
  mle.result
}

source('../lrt_equations/equations.logitXG.R')
list2env(lrt.logitXG.equations, envir = environment())

# source('../lrt_equations/equations.logitG.R')
# list2env(lrt.logitG.equations, envir = environment())

# contour analysis ####

# set parameters
params <- list(
  n = 5e5,
  omega = 1e-3,
  alpha = -2.2,
  beta = 5e-1,
  gamma = 0.8,
  eta = -0.4,
  cases.only = T
)

# simulate data
sim.result <- f.call(sim, params)

mle.result <- do.mle(params, sim.result, L.ALT, D); mle.result
f.call(LR, modifyList(
       modifyList(params, sim.result),
       list(gamma = mle.result$par[1], eta = mle.result$par[2], cases.only = T)
))

# table(sim.result$G, sim.result$Y)
# grp1.idx <- which(sim.result$Y == 1 & sim.result$G == 1)
# grp2.idx <- which(sim.result$Y == 1 & sim.result$G == 0)
# 
# gamma.range <- seq(-2, 2, length.out = 200)
# eta.range <- seq(-2, 2, length.out = 200)
# 
# 
# grp.1.sum <- lapply(sim.result$X[grp1.idx], function(X.val) {
#   f <- function(gamma, eta) {
#     R1(params$omega, params$alpha, params$beta, gamma, eta, X.val)
#   }
#   
#   log(outer(gamma.range, eta.range, f))
# }) %>%
#   Reduce(`+`, .) %>%
#   magrittr::set_rownames(gamma.range) %>%
#   magrittr::set_colnames(eta.range)
# 
# grp.2.sum <- lapply(sim.result$X[grp2.idx], function(X.val) {
#   f <- function(gamma, eta) {
#     R2(params$omega, params$alpha, params$beta, gamma, eta, X.val)
#   }
#   
#   log(outer(gamma.range, eta.range, f))
# }) %>%
#   Reduce(`+`, .) %>%
#   magrittr::set_rownames(gamma.range) %>%
#   magrittr::set_colnames(eta.range)
# 
# all.sum <- grp.1.sum + grp.2.sum; max(all.sum)
# i <- which(all.sum == max(all.sum), arr.ind = T)
# gamma.range[i[1]]
# eta.range[i[2]]
# 
# filled.contour(
#   list(x = gamma.range, y = eta.range, z = grp.1.sum + grp.2.sum)
#   # color.palette = colorRampPalette(c("red", "white", "green"))
# )
# 
# f.call(L.ALT, modifyList(modifyList(params, sim.result),
#                   list(params = c(gamma.range[i[1]], eta.range[i[2]]))))



# filled.contour(
#   list(x = gamma.range, y = eta.range, z = grp.1.sum),
#   color.palette = colorRampPalette(c("red", "white", "green"))
# )
# 
# filled.contour(
#   list(x = gamma.range, y = eta.range, z = grp.2.sum),
#   color.palette = colorRampPalette(c("red", "white", "green"))
# )


grp.1.sum %>%
  as.data.frame %>%
  mutate(gamma = rownames(.)) %>%
  pivot_longer(cols = -c('gamma'), names_to = 'eta', values_to = 'log.likelihood') %>%
  mutate(
    gamma = as.numeric(gamma),
    eta = as.numeric(eta)
  ) %>%
  ggplot(aes(x = gamma, y = eta, z = log.likelihood)) +
  geom_contour_filled()

as.table(grp.1.sum)

R1(0.001, -2.2, 0.5, c(0.1, 0.5, 0.6), c(-0.1, 0, 0.1), c(-1, 0, 1))

table(sim.result$G, sim.result$Y)

f.call(ll.alt, modifyList(params, sim.result))

params.test.logitXG <- list(
  omega = 0.001,
  alpha = -2.2,
  beta = 0.5,
  # gamma = 0,
  eta = 0,
  X = -1
)

data.frame(
  # X = seq(-20, 20, length.out = 1000)
  gamma = seq(-3, 3, length.out = 1000)
) %>%
  mutate(
    # R1 = f.call(R1, modifyList(params.test.logitXG, list(X = X))),
    R2 = f.call(R2, modifyList(params.test.logitXG, list(gamma = gamma))),
    rXY1 = exp(f.call(rXY1, modifyList(params.test.logitXG, list(gamma = gamma)))),
    DR1Gamma = f.call(DR1Gamma, modifyList(params.test.logitXG, list(gamma = gamma))),
    DR1Eta = f.call(DR1Eta, modifyList(params.test.logitXG, list(gamma = gamma))),
    DR2Gamma = f.call(DR2Gamma, modifyList(params.test.logitXG, list(gamma = gamma))),
    DR2Eta = f.call(DR2Eta, modifyList(params.test.logitXG, list(gamma = gamma))),
  ) %>%
  pivot_longer(cols = -c('gamma'), names_to = 'f', values_to = 'val') %>%
  # ggplot(aes(x = X, y = val, col = 'f')) +
  ggplot(aes(x = gamma, y = val)) +
  facet_wrap(~f, scales = 'free') +
  geom_line()

sim.result <- f.call(sim, params)

f.call(ll.alt, modifyList(modifyList(params, sim.result), list(gamma = mle.result$par[1], eta = mle.result$par[2])))
f.call(ll.null, modifyList(modifyList(params, sim.result), list(gamma = mle.result$par[1], eta = mle.result$par[2])))

sum(log(
  1 / (1 + exp(-params$alpha - params$beta * sim.result$X[sim.result$Y == 1])) /
  (1 / (1 + exp(-params$alpha)))
))

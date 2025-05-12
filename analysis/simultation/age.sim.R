library(dplyr)
library(ggplot2)

# logistic simulator
age.sim <- function(
  n,
  alpha,
  beta,
  gamma,
  zeta,
  omega,
  kappa
) {
  # genotype
  G <- rbinom(n = n, size = 1, prob = omega)
  # prs
  X <- rnorm(n = n, mean = 0, sd = 1)
  # age or another continuous normal confounder
  A <- rnorm(n = n, mean = 0, sd = 1)
  
  # interaction
  AX <- A * X
  
  pY <- alpha + beta * X + gamma * G + zeta * A + kappa * AX
  
  Y <- rbinom(n = n, size = 1, prob = expit(pY))
  
  list(
    Y = Y,
    X = X,
    G = G,
    A = A
  )
}

age.params <- list(
  n = 5e5,
  omega = 1e-3,
  alpha = -2.2,
  gamma = 1.2,
  beta = 0.3,
  zeta = 0.9,
  kappa = 1
)

broom::tidy(glm(Y ~ X * G, family = binomial(link = 'logit'), data = f.call(age.sim, age.params)))

kakashi <- lapply(seq(100), function(i) {
  print(i)
  broom::tidy(glm(Y ~ X * G, family = binomial(link = 'logit'), data = f.call(age.sim, age.params)))
}) %>% do.call(bind_rows, .)

kakashi %>%
  filter(term == "X:G")
  # mutate(sig = p.value < 0.05) %>%
  # filter(sig)
  # summarize(n = n(), m = mean(sig))
  # ungroup() %>%
  # View()
  

params.grid <- expand.grid(
  gamma = seq(0, 1, 1/2),
  beta = seq(0, 1, 1/2),
  zeta = seq(0, 1, 1/2)
)

age.interaction.results <- lapply(seq(33), function(i) {
  print(i)
  mapply(function(gamma, beta, zeta) {
    age.sim.result <- tibble::as_tibble(
      f.call(age.sim,
            modifyList(age.params, list(
              gamma = gamma,
              beta = beta,
              zeta = zeta
    ))))
    my.model <- broom::tidy(glm(Y ~ X * G, family = binomial(link = 'logit'), data = age.sim.result))
    my.model$gamma <- gamma
    my.model$beta <- beta
    my.model$zeta <- zeta
    my.model
  }, params.grid$gamma, params.grid$beta, params.grid$zeta, SIMPLIFY = F) %>%
    do.call(dplyr::bind_rows, .)
}) %>%
    do.call(dplyr::bind_rows, .)

age.interaction.results %>%
  filter(term == 'X:G') %>%
  filter(p.value < 0.05) %>%
  group_by(beta, gamma, zeta) %>%
  summarize(
    n = n(),
    mean.est = mean(estimate),
    sd.est = sd(estimate),
    mean.p.val = mean(p.value),
    sd.p.val = sd(p.value)
  ) %>%
  ungroup() %>%
  arrange(desc(n))

naruto <- age.interaction.results %>%
  filter(term == 'X:G') %>%
  group_by(beta, gamma, zeta) %>%
  summarize(
    mean.est = mean(estimate),
    sd.est = sd(estimate),
    mean.p.val = mean(p.value),
    sd.p.val = sd(p.value)
  ) %>%
  ungroup()

broom::tidy(lm(mean.p.val ~ beta + gamma + zeta, data = naruto))

source('../lrt_equations/equations.logitG.R')
list2env(lrt.logitG.equations, envir = environment())

f.lrt <- function(params) {
  sim.result <- f.call(a.sim, params)
  x <- as.data.frame(sim.result)
  
  lrt.cc <- run.lrt(sim.result, params, cases.only = F)
  lrt.co <- run.lrt(sim.result, params, cases.only = T)
  
  # fwd.p.val <- lmtest::lrtest(
  #   glm(Y ~ 1 + X + G + GX, family = binomial(link = 'logit'), data = x),
  #   glm(Y ~ 1 + X, family = binomial(link = 'logit'), data = x)
  # )[2,5]
  
  wilcox.p.val <- wilcox.test(
    X ~ G,
    data = x[x$Y==1,],
    alternative = 'greater'
  )$p.value
  
  setNames(lrt.co, paste('co', names(lrt.co), sep='.')) %>%
    modifyList(setNames(lrt.cc, paste('cc', names(lrt.cc), sep='.'))) %>%
    modifyList(
      list(
        # fwd.p.val = fwd.p.val,
        wilcox.p.val = wilcox.p.val
      )
    ) %>%
    modifyList(z.test(w = params$omega, sim.result))
}

params <- list(
  n = 5e5,
  omega = 0.001,
  alpha = -2.2,
  eta = 0,
  beta = 0.6,
  zeta = 1.2,
  kappa = 0.4,
  lambda = 0
)

n.sims <- 32

lrt.ranges <- expand.grid(
  gamma = seq(2, 2, length.out = 1),
  zeta = seq(1.2, 1.2, length.out = 1),
  kappa = seq(0, 1, length.out = 2),
  # lambda = seq(0, 1, length.out = 3)
  lambda = c(0)
)

lrt.result <- mapply(function(gamma, zeta, kappa, lambda) {
  params$gamma <- gamma
  params$zeta <- zeta
  params$kappa <- kappa
  params$lambda <- lambda
  
  run.lrt.sims(f.lrt, params, n.sims = n.sims)
},
lrt.ranges$gamma,
lrt.ranges$zeta,
lrt.ranges$kappa,
lrt.ranges$lambda,
SIMPLIFY = F
) %>%
  do.call(bind_rows, .) %>%
  as_tibble()

# saveRDS(lrt.result, file.path(out.dir, 'logitG.lrt.result.rds'))

lrt.result %>%
  mutate(
    param.gamma = as.factor(param.gamma),
    param.kappa = as.factor(param.kappa),
    param.lambda = as.factor(param.lambda),
  ) %>%
  group_by(param.gamma, param.kappa, param.lambda) %>%
  summarize(mu = mean(cc.eta), s = sd(cc.eta), m = median(cc.eta))

lrt.result %>%
  mutate(
    param.zeta = as.factor(param.zeta),
    param.kappa = as.factor(param.kappa),
    param.lambda = as.factor(param.lambda),
  ) %>%
  ggplot(aes(x = cc.eta, color = param.zeta)) +
  geom_density() +
  facet_grid(rows = vars(param.kappa), cols = vars(param.lambda))
library(rootSolve)
library(parallel)

expit <- function(logit) {
  1 / (1 + exp(-logit))
  # exp(logit) / (1 + exp(logit))
}

logitY <- function(alpha, beta, gamma, eta, X, G) {
  alpha + beta * X + gamma * G + eta * X * G
}

pY <- function(alpha, beta, gamma, eta, X, G) {
  expit(logitY(alpha, beta, gamma, eta, X, G))
}

n.cores <- ceiling(parallel::detectCores() / 2)
n.batch <- 64

# logistic simulator
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
  
a.sim <- function(
  n,
  omega,
  alpha,
  gamma,
  beta,
  zeta,
  eta = 0,
  kappa = 0,
  lambda = 0
) {
  # genotype
  G <- rbinom(n = n, size = 1, prob = omega)
  # prs
  X <- rnorm(n = n, mean = 0, sd = 1)
  # age or another continuous normal confounder
  A <- rnorm(n = n, mean = 0, sd = 1)
  GX <- G * X
  AX <- A * X
  AG <- A * G
  
  pY <- alpha + beta * X + gamma * G + zeta * A + eta * GX + kappa * AX + lambda * AG
  
  Y <- rbinom(n = n, size = 1, prob = expit(pY))
  
  list(
    Y = Y,
    X = X,
    G = G,
    A = A
  )
}

extract.args.for.function <- function(f, args.list) {
  formal.args <- names(formals(f))
  args.list[names(args.list) %in% formal.args]
}

f.call <- function(f, args.list, envir = parent.frame(), force.incl = c()) {
  args.list <- modifyList(
    extract.args.for.function(f, args.list),
    args.list[names(args.list) %in% force.incl]
  )
  do.call(f, args.list, envir = envir)
}

z.test <- function(w, sim.result) {
  n.Y1 <- sum(sim.result$Y)
  n.Y1G1 <- sum(sim.result$Y & sim.result$G)
  
  z <- (n.Y1G1 - (n.Y1 * w)) / sqrt(w * (1 - w) * n.Y1)
  # z.p.val <- 2 * (1 - pnorm(abs(z)))
  z.p.val <- 1 - pnorm(z)
  
  list(
    z = z,
    z.p.val = z.p.val
  )
}

fit.sigmoid <- function(x, y, res.fit) {
  sigmoid <- function(x, a, b, c) {
    1 / (1 + exp(-(a * x + b))) + c
  }
  
  my.fit <- tryCatch({
    nls(
      y ~ sigmoid(x, a, b, c),
      data = data.frame(x = x, y = y),
      algorithm = 'port',
      start = list(a = 10, b = -1, c = 0)
    )
  }, error = function(e) {
    lm(y ~ poly(x, 3))
  })
  
  fit.df <- data.frame(x = seq(min(x), max(x), res.fit))
  fit.df$y <- predict(my.fit, newdata = fit.df)
  
  fit.df
}

mle.root <- function(f, f.params, start, maxiter=50) {
  root.params <- list(
    f = f,
    start = start,
    maxiter = maxiter
  )

  root.result <- f.call(multiroot, c(f.params, root.params), force.incl = names(f.params))
  c(root.result$root, root.result$iter)
}

mle.optim <- function(f, d, f.params, start) {
  optim.params <- list(
    par = start,
    fn = f,
    gr = d,
    method = "BFGS",
    # method = "SANN",
    control = list(
      maxit = 256,
      fnscale = -1
    )
  )
  
  f.call(optim, c(f.params, optim.params), force.incl = names(f.params))
}

mle <- function(lrt.equations, lrt.data, params) {
  f.params <- modifyList(params, lrt.data)
  f.params <- extract.args.for.function(lrt.equations$D, f.params)
  
  start <- c(
    rnorm(1, params$gamma, 0.3),
    rnorm(1, params$eta, 0.3)
  )
  
  optimum <- mle.optim(lrt.equations$L, lrt.equations$D, f.params, start)
  optim.result <- list(gamma = optimum$par[1], eta = optimum$par[2], iter = sum(optimum$counts))
  # root <- mle.root(D, f.params, start = start, maxiter = maxiter)
  # root.result <- list(gamma = root[1], eta = root[2], iter = root[3])
  
  if (!sum(unlist(purrr::map(optim.result, ~is.nan(.))))) {
    return(optim.result)
  }
  
  return(NULL)
}

run.lrt <- function(lrt.equations, lrt.data, params) {
  # solve MLE for gamma and eta (Cases Only)
  mle.params <- list(
    lrt.equations = lrt.equations,
    lrt.data = lrt.data,
    params = params
  )
  mle.result <- f.call(mle, mle.params)
  
  if (is.null(mle.result)) {
    return(list(
      iter = NA,
      gamma.est = NA,
      eta.est = NA,
      ll = NA,
      p.val = 1
    ))
  }
  
  # calculate lrt test statistic and p-value
  ll.params <- modifyList(
    lrt.data,
    modifyList(
      params,
      mle.result # this overrides gamma and eta w/ the new values
    )
  )
  
  ll <- f.call(lrt.equations$LR, ll.params)
  chi2 <- 2 * ll
  p.val <- pchisq(chi2, df = 2, lower.tail = F)
  
  modifyList(mle.result, list(ll = ll, lrt.p.val = p.val))
}

run.lrt.sims <- function(f.lrt, params, n.sims) {

  n.complete <- 0
  lrt.result <- data.frame()
  # run in batches of n.batch until n.sims complete
  while (n.complete < n.sims) {
    n.to.run <- min(n.batch, n.sims - n.complete)

    # run n.to.run sims/tests
    lrt.batch <- parallel::mclapply(seq(n.sims), function(i) {
      f.lrt(params)
    }, mc.cores = n.cores)
    
    errors <- purrr::keep(lrt.batch, function(e) inherits(e, 'try-error'))
    if (length(errors)) {
      stop(errors[!duplicated(as.character(errors))])
    }
    
    n.complete <- n.complete + n.to.run

    lrt.result <- bind_rows(lrt.result, bind_rows(lrt.batch))
  }
  
  lrt.result
}


# run function lrt.func n.sims number of times using mle.params modified over params.ranges
run.sim.ranges <- function(lrt.func, mle.params, params.ranges, n.sims = 1024) {
  lrt.result <- apply(params.ranges, 1, function(grid.params) {
    run.params <- modifyList(mle.params, as.list(grid.params))
    run.lrt.sims(lrt.func, run.params, n.sims) %>%
      modifyList(setNames(run.params, paste0('param.', names(run.params))))
  }) %>%
    do.call(bind_rows, .) %>%
    as_tibble()
}

library(rootSolve)

n.cores <- ceiling(detectCores() / 2)
n.batch <- 128

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

fit.sigmoid <- function(x, y, res.fit = 0.01) {
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

expit <- function(logit) {
  exp(logit) / (1 + exp(logit))
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

mle.optim <- function(f, d, f.params, start, maxit=100) {
  optim.params <- list(
    par = start,
    fn = f,
    gr = d,
    method = "BFGS",
    control = list(
      maxit = maxit,
      fnscale = -1
    )
  )
  
  f.call(optim, c(f.params, optim.params), force.incl = names(f.params))
}

lrt <- function(f.ll, ll.params) {
  # calculate log likelihood
  ll <- f.call(f.ll, ll.params)
  # calculate liklihood ratio test statistic
  # chi2 <- -2 * ll
  chi2 <- 2 * ll
  # return p.value
  pchisq(chi2, df = 2, lower.tail = F)
}

mle <- function(sim.result, params, maxiter = 50, cases.only = T) {
  f.params <- modifyList(params, sim.result)
  f.params <- extract.args.for.function(D, f.params)
  f.params <- modifyList(f.params, list(cases.only = cases.only))
  
  start <- c(
    rnorm(1, params$gamma, 0.3),
    rnorm(1, params$eta, 0.3)
  )
  
  optimum <- mle.optim(L, D, f.params, start, maxit = maxiter)
  optim.result <- list(gamma = optimum$par[1], eta = optimum$par[2], iter = sum(optimum$counts))
  # root <- mle.root(D, f.params, start = start, maxiter = maxiter)
  # root.result <- list(gamma = root[1], eta = root[2], iter = root[3])
  
  if (!sum(unlist(purrr::map(optim.result, ~is.nan(.))))) {
    return(optim.result)
  }
  
  return(NULL)
}

run.mle <- function(sim.result, params, cases.only = T) {
  # solve MLE for gamma and eta (Cases Only)
  root.params <- list(
    sim.result = sim.result,
    params = params,
    cases.only = cases.only
  )
  root.result <- f.call(mle, root.params)
  
  if (is.null(root.result)) {
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
    sim.result,
    modifyList(
      params,
      root.result # this overrides gamma and eta w/ the new values
    )
  )
  ll.params$cases.only <- cases.only
  
  ll <- f.call(LL, ll.params)
  chi2 <- 2 * ll
  p.val <- pchisq(chi2, df = 2, lower.tail = F)
  
  modifyList(root.result, list(ll = ll, lrt.p.val = p.val))
}

run.lrt <- function(f.lrt, params, n.sims) {
  print(unlist(purrr::map(params, ~ format(., scientific = F))))
  
  n.complete <- 0
  lrt.result <- data.frame()
  # run in batches of n.batch until n.sims complete
  while (n.complete < n.sims) {
    n.to.run <- min(n.batch, n.sims - n.complete)
    
    # run n.to.run sims/tests
    lrt.batch <- mclapply(seq(n.to.run), function(i) {
      f.lrt(params)
    }, mc.cores = n.cores) %>%
      do.call(bind_rows, .)
    
    n.complete <- n.complete + n.to.run
    
    lrt.result <- bind_rows(lrt.result, lrt.batch)
  }
  
  lrt.result
}

save.plot <- function(my.plot, file.prefix, root.dir = getwd(), sub.dir = '', w = 7.35 * 2, h = 4.5 * 2) {
  # save plot (defaults to last_plot())
  #
  # @param filename   filename for save
  # @param plot       plot to save
  # @param dir        directory for save
  # @param w          plot width
  # @param h          plot height
  
  dir.create(file.path(root.dir, 'svg', sub.dir), showWarnings = F, recursive = T)
  dir.create(file.path(root.dir, 'png', sub.dir), showWarnings = F, recursive = T)
  dir.create(file.path(root.dir, 'pdf', sub.dir), showWarnings = F, recursive = T)
  
  ggsave(filename = file.path(root.dir, 'svg', sub.dir, paste0(file.prefix, '.svg')), plot = my.plot, width = w, height = h)
  ggsave(filename = file.path(root.dir, 'png', sub.dir, paste0(file.prefix, '.png')), plot = my.plot, width = w, height = h)
  pdf(file = file.path(root.dir, 'pdf', sub.dir, paste0(file.prefix, '.pdf')), width = w, height = h)
  print(my.plot)
  x <- dev.off()
  my.plot
}

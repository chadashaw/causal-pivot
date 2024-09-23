library(parallel, quietly = T, warn.conflicts = F)
library(lmtest, quietly = T, warn.conflicts = F)
source('utils.R')

args <- commandArgs(trailingOnly = TRUE)

model.name <- args[1]
gamma.val <- as.numeric(args[2])
eta.val <- as.numeric(args[3])
n.sims <- as.numeric(args[4])

out.file <- ifelse(!is.na(args[5]), args[5], '/app/results/lrt.result.rds')
dir.create(dirname(out.file), showWarnings = F, recursive = T)

# model.name <- 'logitG'
# gamma.val <- 0
# eta.val <- 0
# n.sims <- 64

source(paste0(c('equations', model.name, 'R'), collapse = '.'))
list2env(equations, envir = environment())

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

run.lrt <- function(sim.result, params, cases.only = T) {
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
      gamma = NA,
      eta = NA,
      ll.result = NA,
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
  
  ll.result <- f.call(LL, ll.params)
  chi2 <- 2 * ll.result
  p.val <- pchisq(chi2, df = 2, lower.tail = F)
  
  modifyList(root.result, list(ll.result = ll.result, p.val = p.val))
}

run.tests <- function(sim.result, params) {
  lrt.co <- run.lrt(sim.result, params, cases.only = T)
  lrt.cc <- run.lrt(sim.result, params, cases.only = F)
   
  x <- data.frame(sim.result)
  
  if (model.name == 'logitG') {
    x.co <- x[x$Y == 1,]
    
    fwd.p.val = lmtest::lrtest(
      glm(Y ~ 1 + X + G + GX, family = binomial(link = 'logit'), data = x),
      glm(Y ~ 1 + X, family = binomial(link = 'logit'), data = x)
    )[2,5]
    
    rev.co.p.val = lmtest::lrtest(
      glm(G ~ 1 + X, family = binomial(link = "logit"), data = x.co),
      glm(G ~ 1, family = binomial(link = "logit"), data = x.co)
    )[2,5]
  } else if (model.name == 'liabilityG') {
    x.co <- x[x$y >= params$delta,]
    
    fwd.p.val = lmtest::lrtest(
      lm(y ~ 1 + X + G + GX, data = x),
      lm(y ~ 1 + X, data = x)
    )[2,5]
    
    rev.co.p.val = lmtest::lrtest(
      glm(G ~ 1 + X + y + y*X, family = binomial(link = "logit"), data = x.co),
      glm(G ~ 1, family = binomial(link = "logit"), data = x.co)
    )[2,5]
  # } else if (model.name == 'burdenX') {
  #   lrt.cc <- NULL
  #   x.co <- x[x$Y == 1,]
  #   
  #   fwd.p.val = lmtest::lrtest(
  #     glm(Y ~ 1 + X + G + GX, family = binomial(link = 'logit'), data = x),
  #     glm(Y ~ 1 + X, family = binomial(link = 'logit'), data = x)
  #   )[2,5]
  #   
  #   rev.co.p.val = lmtest::lrtest(
  #     lm(X ~ 1 + G, data = x.co),
  #     lm(X ~ 1, data = x.co)
  #   )[2,5]
  }
  else {
    fwd.p.val <- NA
    rev.co.p.val <- NA
  }
  
  list(
    iter.co = lrt.co$iter,
    gamma.co = lrt.co$gamma,
    eta.co = lrt.co$eta,
    ll.co = lrt.co$ll.result,
    ll.co.p.val = lrt.co$p.val,
    iter.cc = lrt.cc$iter,
    gamma.cc = lrt.cc$gamma,
    eta.cc = lrt.cc$eta,
    ll.cc = lrt.cc$ll.result,
    ll.cc.p.val = lrt.cc$p.val,
    fwd.p.val = fwd.p.val,
    rev.co.p.val = rev.co.p.val
  )
}

n.cores <- ceiling(detectCores() / 2) # safety for cloud env
print(paste('using', n.cores, 'cores'))
n.batch <- 64

lrt.result <- data.frame()
print(paste('gamma=', gamma.val, 'eta=', eta.val))
params <- modifyList(params.0, list(gamma = gamma.val, eta = eta.val))

# run in batches of n.batch until n.sims complete
while (nrow(lrt.result) < n.sims) {
  n.to.run <- min(n.batch, n.sims - nrow(lrt.result))
  print(paste('running', n.to.run, 'sims'))
  
  # run n.to.run sims/tests
  lrt.batch <- mclapply(seq(n.to.run), function(i) {
    sim.result <- f.call(sim, params)
    run.tests(sim.result, params)
  }, mc.cores = n.cores)
  
  # filter out invalid results
  lrt.batch <- Filter(is.list, lrt.batch)
  
  # fix NULL fields
  lrt.batch <- lapply(lrt.batch, function(l) {
    lapply(l, function(f) {
      ifelse(is.null(f), NA, f)
    })
  })
  
  # massage to dataframe
  lrt.batch <- do.call(rbind, lapply(lrt.batch, as.data.frame))
  
  # append to results
  lrt.result <- rbind(lrt.result, lrt.batch)
}

lrt.result$gamma <- gamma.val
lrt.result$eta <- eta.val

saveRDS(lrt.result, out.file)

library(rootSolve)

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
  # optim.params <- list(
  #   par = start,
  #   fn = f,
  #   gr = d,
  #   method = "BFGS",
  #   control = list(
  #     maxit = maxit,
  #     fnscale = -1
  #   )
  # )
  
  optim.params <- list(
    par = start,
    fn = f,
    # gr = d,
    method = "SANN",
    control = list(
      maxit = maxit,
      fnscale = -1
    )
  )
  
  f.call(optim, c(f.params, optim.params), force.incl = names(f.params))
}

mle <- function(lrt.data, params, maxiter = 50, cases.only = T) {
  # add data to estimated model parameters
  f.params <- modifyList(params, lrt.data)
  # extract only those parameters needed for the optimizer
  # note: this ensures parameters to be estimated are actually estimated by multiroot
  f.params <- extract.args.for.function(D, f.params)
  f.params <- modifyList(f.params, list(cases.only = cases.only))
  
  start <- c(
    rnorm(1, params$gamma, 0.3),
    rnorm(1, params$eta, 0.3)
  )
  
  tryCatch({
    optimum <- mle.optim(L, D, f.params, start, maxit = maxiter)
    list(
      gamma = optimum$par[1],
      eta = optimum$par[2],
      iter = sum(optimum$counts),
      iter.f = optimum$counts['function'],
      iter.g = optimum$counts['gradient']
    )
  }, error = function(cond) {
    message('Failed to find MLE')
    message(conditionMessage(cond))
    NULL
  })
  
  # root <- mle.root(D, f.params, start = start, maxiter = maxiter)
  # root.result <- list(gamma = root[1], eta = root[2], iter = root[3])
  # 
  # if (!sum(unlist(purrr::map(root.result, ~is.nan(.))))) {
  #   return(root.result)
  # }
}

run.lrt <- function(lrt.data, params, cases.only=T) {
  # solve MLE for gamma and eta (Cases Only)
  root.params <- list(
    lrt.data = lrt.data,
    params = params,
    cases.only = cases.only
  )
  root.result <- f.call(mle, root.params)
  
  if (is.null(root.result)) {
    return(list(
      iter = NA,
      gamma = NA,
      eta = NA,
      ll.result = NULL,
      p.val = 1
    ))
  }
  
  # calculate lrt test statistic and p-value
  ll.params <- modifyList(
    lrt.data,
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

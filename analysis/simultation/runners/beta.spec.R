result <- local({
  source(file.path(LRT_EQNS_DIR, 'equations.logitG.R'))
  
  # cc.logitG_A.equations <- define.logitG_A.equations(cases.only = F)
  co.logitG.equations <- define.logitG.equations(cases.only = T)
  
  f.lrt.beta <- function(params) {
    sim.result <- f.call(sim, params)
    
    a <- params$a
    
    x <- as.data.frame(sim.result)
    
    if (params$E.omega) {
      E.omega <- (params$omega / (1 - params$omega)) *
        (
          f.call(pY, modifyList(params, list(X = 0, G = 1))) /
            (f.call(pY, modifyList(params, list(X = 0, G = 0))))
        )
      params$omega <- params$omega + a * (E.omega - params$omega)
    } else {
      params$omega <- params$omega + a * (mean(x[x$Y==1,]$G) - params$omega)
    }
    
    # lrt.cc <- run.lrt(sim.result, params, cases.only = F)
    lrt.co <- run.lrt(co.logitG.equations, sim.result, params)
    
    # fwd.p.val <- lmtest::lrtest(
    #   glm(Y ~ 1 + X + G + GX, family = binomial(link = 'logit'), data = x),
    #   glm(Y ~ 1 + X, family = binomial(link = 'logit'), data = x)
    # )[2,5]
    fwd.p.val <- NULL
    
    wilcox.p.val <- wilcox.test(
      X ~ G,
      data = x[x$Y==1,],
      alternative = 'greater'
    )$p.value
    
    setNames(lrt.co, paste('co', names(lrt.co), sep='.')) %>%
      # modifyList(setNames(lrt.cc, paste('cc', names(lrt.cc), sep='.'))) %>%
      modifyList(
        list(
          fwd.p.val = fwd.p.val,
          wilcox.p.val = wilcox.p.val
        )
      ) %>%
      modifyList(z.test(w = params$omega, sim.result)) %>%
      modifyList(list(o.s = params$omega))
  }
  
  params.beta <- list(
    n = 5e5,
    omega = 0.001,
    alpha = -2.2,
    a = 0.9,
    # 1
    eta = -0.1,
    E.omega = T
    # 2
    # eta = -0.1,
    # E.omega = F
    # 3
    # eta = -0.4,
    # E.omega = T
  )
  
  # f.lrt.beta(modifyList(params.beta, list(gamma = 2.2, beta = 0.5)))
  
  gamma.range <- seq(0.0, 2.6, length.out = 9)
  beta.range <- c(0.1, 0.3, 0.5)
  
  lrt.ranges.beta <- expand.grid(gamma = gamma.range, beta = beta.range)
  
  lrt.result <- mapply(function(gamma, beta) {
    params.beta$gamma <- gamma
    params.beta$beta <- beta
    
    r <- run.lrt.sims(f.lrt.beta, params.beta, n.sims = n.sims)
    
    r$param.gamma <- params.beta$gamma
    r$param.omega <- params.beta$omega
    r$param.alpha <- params.beta$alpha
    r$param.beta <- params.beta$beta
    r$param.eta <- params.beta$eta
    
    r
  }, lrt.ranges.beta$gamma, lrt.ranges.beta$beta, SIMPLIFY = F) %>%
    do.call(bind_rows, .) %>%
    as_tibble()

  list(
    equations = co.logitG.equations,
    params = params.beta,
    lrt.fun = f.lrt.beta,
    n.sims = n.sims,
    lrt.result = lrt.result
  )
})

source(file.path(LRT_EQNS_DIR, 'equations.logitG.R'))

result <- local({
  # cc.logitG.equations <- define.logitG.equations(cases.only=F)
  co.logitG.equations <- define.logitG.equations(cases.only=T)
  
  logitG.lrt <- function(params) {
    sim.result <- f.call(sim, params)
    x <- as.data.frame(sim.result)
    
    # lrt.cc <- run.lrt(cc.logitG.equations, sim.result, params)
    lrt.co <- run.lrt(co.logitG.equations, sim.result, params)
    
    fwd.p.val <- lmtest::lrtest(
      glm(Y ~ 1 + X + G + GX, family = binomial(link = 'logit'), data = x),
      glm(Y ~ 1 + X, family = binomial(link = 'logit'), data = x)
    )[2,5]
    
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
      modifyList(z.test(w = params$omega, sim.result))
  }
  
  logitG.params <- list(
    n = 5e5,
    omega = 0.001,
    alpha = -2.2,
    beta = 0.6
  )
  
  logitG.param.ranges <- expand.grid(
    gamma = seq(0.0, 1.2, by = 0.2),
    eta = c(-0.4, 0)
  )
  
  lrt.result <- run.sim.ranges(logitG.lrt, logitG.params, logitG.param.ranges, n.sims = n.sims)
  
  list(
    equations = co.logitG.equations,
    params = logitG.params,
    lrt.fun = logitG.lrt,
    n.sims = n.sims,
    lrt.result = lrt.result
  )
})

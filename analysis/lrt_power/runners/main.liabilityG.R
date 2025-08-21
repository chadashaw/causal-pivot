source(file.path(LRT_EQNS_DIR, 'equations.liabilityG.R'))

result <- local({
  liabilityG.equations <- define.liabilityG.equations(cases.only=T)
  
  f.lrt.liability <- function(params) {
    sim.result <- f.call(liabilityG.equations$sim, params)
    
    lrt.co <- run.lrt(liabilityG.equations, sim.result, params)
     
    x <- data.frame(sim.result)
    
    fwd.p.val = lmtest::lrtest(
      lm(y ~ 1 + X + G + GX, data = x),
      lm(y ~ 1 + X, data = x)
    )[2,5]
    
    wilcox.p.val <- wilcox.test(
      X ~ G,
      data = x[x$y >= params$delta,],
      alternative = 'greater'
    )$p.value
    
    sim.result$Y <- sim.result$y >= params$delta # for z-test
    
    setNames(lrt.co, paste('co', names(lrt.co), sep='.')) %>%
      modifyList(
          list(
            fwd.p.val = fwd.p.val,
            wilcox.p.val = wilcox.p.val
          )
      ) %>%
        modifyList(z.test(w = params$omega, sim.result))
  }
  
  liabilityG.params <- list(
    n = 500000,
    omega = 0.001,
    alpha = 0,
    beta = 0.3,
    gamma = 0.0,
    eta = 0,
    delta = 1.33,
    sige = 1
  )
  
  liabilityG.param.ranges <- expand.grid(
    gamma = seq(0.0, 1.2, length.out = 9),
    eta = c(-0.4, 0)
  )
  
  lrt.result <- run.sim.ranges(f.lrt.liability, liabilityG.params, liabilityG.param.ranges, n.sims = n.sims)
  
  list(
    equations = liabilityG.equations,
    params = liabilityG.params,
    lrt.fun = f.lrt.liability,
    n.sims = n.sims,
    lrt.result = lrt.result
  )
})
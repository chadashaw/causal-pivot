source(file.path(LRT_EQNS_DIR, 'equations.logitG.R'))

result <- local({
  # cc.logitG.equations <- define.logitG.equations(cases.only=F)
  co.logitG.equations <- define.logitG.equations(cases.only=T)
  
  o.params <- list(
    n = 5e5,
    omega = 0.001,
    alpha = -2.2,
    beta = 0.6,
    gamma = 2.2,
    eta = 0
  )
  
  o.param.ranges <- expand.grid(
    omega = seq(0.001, 0.006, by = 0.0005)
  )
  
  o.f.lrt <- function(params) {
    sim.result <- f.call(sim, params)
    
    n.Y1 <- sum(sim.result$Y)
    n.Y1G1 <- sum(sim.result$Y & sim.result$G)
    o <- n.Y1G1 / n.Y1
    
    lapply(o.param.ranges$omega, function(omega) {
      if (omega <= 0) {
        return(NULL)
      }
      
      lrt.co <- run.lrt(co.logitG.equations, sim.result, modifyList(params, list(omega = omega)))
      z = (n.Y1G1 - (n.Y1 * omega)) / sqrt(omega * (1 - omega) * n.Y1)
      
      setNames(lrt.co, paste('co', names(lrt.co), sep='.')) %>%
        modifyList(
          list(
            n.Y1 = n.Y1,
            n.Y1G1 = n.Y1G1,
            o = o,
            param.omega = omega,
            z = z,
            z.p.val = 1 - pnorm(z)
          )
        )
    }) %>%
      do.call(bind_rows, .)
  }
  
  # wilcox.1t.p.vals <- mclapply(seq(1024), function(i) {
  #   sim.result <- f.call(sim, o.params)
  #   
  #   wilcox.test(
  #     X ~ G,
  #     data = filter(as.data.frame(sim.result), Y == 1),
  #     alternative = 'greater'
  #   )$p.value
  # }, mc.cores = n.cores) 
  # 
  # wilcox.1t.power <- mean(wilcox.1t.p.vals < 0.05)
  
  lrt.result <- run.lrt.sims(o.f.lrt, o.params, n.sims = n.sims) %>%
    as_tibble()
  
  list(
    equations = co.logitG.equations,
    params = o.params,
    lrt.fun = o.f.lrt,
    n.sims = n.sims,
    lrt.result = lrt.result
  )
})

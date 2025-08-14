source(file.path(LRT_EQNS_DIR, 'equations.logitG.R'))

result <- local({
  co.logitG.equations <- define.logitG.equations(cases.only=T)
  
  omega.e.range <- seq(0.0002, 0.0012, by = 0.0001)
  
  f.lrt.enriched.omega <- function(params) {
    sim.result <- f.call(sim, params)
    
    n.Y1 <- sum(sim.result$Y)
    n.Y1G1 <- sum(sim.result$Y & sim.result$G)
    o <- n.Y1G1 / n.Y1
    
    s <- sqrt((params$omega * (1 - params$omega)) / n.Y1)
    
    lapply(omega.e.range, function(omega) {
      params$omega <- omega
      
      lrt.co <- run.lrt(co.logitG.equations, sim.result, params)
      
      setNames(lrt.co, paste('co', names(lrt.co), sep='.')) %>%
        modifyList(
          list(
            n.Y1 = n.Y1,
            n.Y1G1 = n.Y1G1,
            o = o,
            s = s,
            omega = omega,
            z = (n.Y1G1 - (n.Y1 * omega)) / sqrt(omega * (1 - omega) * n.Y1)
          )
        )
    }) %>%
      do.call(bind_rows, .)
  }
  
  params.e <- list(
    n = 5e5,
    omega = 0.001,
    alpha = -2.2,
    beta = 0.6,
    gamma = 0,
    eta = 0
  )
  
  lrt.result <- run.lrt.sims(f.lrt.enriched.omega, params = params.e, n.sims = n.sims) %>%
    as_tibble()
  
  list(
    equations = co.logitG.equations,
    params = params.e,
    lrt.fun = f.lrt.enriched.omega,
    n.sims = n.sims,
    lrt.result = lrt.result
  )
})

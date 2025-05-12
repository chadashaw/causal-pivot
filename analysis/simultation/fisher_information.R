params$gamma <- 0.75

a <- mclapply(seq(128), function(i) {
  sim.result <- f.call(sim, params)
  f.call(grp.fisher, modifyList(params, sim.result))
}, mc.cores = n.cores)

b <- lapply(a, function(i) {
  i$I1 * i$n1 + i$I2 * i$n2
}) %>%
  lapply(function(i) {
    inv(i)
  })

lapply(a, function(i) {
  det(i$I1 * i$n1 + i$I2 * i$n2) / 
    det(i$I1 * i$n1 + i$I2 * i$n2 + i$I3 * i$n3 + i$I4 * i$n4)
}) %>%
  unlist() %>%
  boxplot()

c <- Reduce("+", b) / length(b)

qnorm(0.025) * sqrt(c[1,1])
-1 * (qnorm(0.025) * sqrt(c[1,1]))
  
boxplot(unlist(lapply(a, function(i) det(i$I1) / det(i$I1 + i$I2))))
lapply(a, function(i) det(i$I1 ) / det(i$I1 + i$I2))

boxplot(unlist(lapply(a, function(i) det(i$I1 + i$I2) / det(i$I1 + i$I2 + i$I3 + i$I4))))

table(abs(lrt.result$param.gamma - 0.514285714285714) < 0.00001)

var(
  lrt.result[
    abs(lrt.result$param.gamma - 0.514285714285714) < 0.00001 & lrt.result$param.eta == -0.4,
  ]$co.gamma
)

var(
  lrt.result[
    abs(lrt.result$param.gamma - 0.514285714285714) < 0.00001 & lrt.result$param.eta == -0.4,
  ]$co.eta
)

cov(
  lrt.result[
    abs(lrt.result$param.gamma - 0.514285714285714) < 0.00001 & lrt.result$param.eta == -0.4,
  ]$co.gamma,
  lrt.result[
    abs(lrt.result$param.gamma - 0.514285714285714) < 0.00001 & lrt.result$param.eta == -0.4,
  ]$co.eta
)

# understanding stuff ####

my.thing <- function(u.true, sd, n) {
  obs.range <- rnorm(n, mean = u.true, sd = sd)
  u.range <- seq(-6, 16, length.out = 1000)
  ll.1 <- sapply(u.range, function(u) {
    sapply(obs.range, function(q) dnorm(q, mean=u, sd = sd))
  }) %>%
    magrittr::set_colnames(u.range) %>%
    as_tibble() %>%
    mutate(i = row_number()) %>%
    select(i, everything()) %>%
    pivot_longer(cols = -c('i'), names_to = 'u', values_to = 'p') %>%
    mutate(u = as.numeric(u)) %>%
    mutate(sd = sd)

}

ll.1 <- my.thing(5, 5, 25)
ll.2 <- my.thing(5, 1, 25)

bind_rows(ll.1, ll.2) %>%
  mutate(sd = factor(sd, levels = c('5', '1'))) %>%
  ggplot(aes(x = u, y = log(p), group = i)) +
  theme_bw() +
  facet_wrap(~sd) +
  geom_line(linewidth = 0.2) +
  scale_y_continuous(limits = c(-12, 0), breaks = seq(-12, 0, by = 2)) +
  scale_x_continuous(breaks = seq(-6, 16, by = 2)) +
  geom_vline(xintercept = 5, linetype = 'dotted')

my.thing(rnorm(20, mean = 5, sd = 5))

seq(-6, 12, length.out = 100) %>%
  plot(
    .,
    log(sapply(
      .,
      function(u) dnorm(0, mean = u, sd = 25)
    )),
    ylim = c(-12, 0)
  )

plot(
  seq(-1000, 1000, length.out = 100),
  log(sapply(
    u.range,
    function(u) dnorm(10, mean = u, sd = 5)
  )),
  ylim = c(-12, 0)
)

dim

my.sd <- 1
rnorm(20, mean = 5, sd = my.sd) %>%
  sapply(function(y) dnorm(y, mean = 5, sd = my.sd)) %>%
  log() %>%
  density() %>%
  plot()

                
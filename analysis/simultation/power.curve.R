RESULTS_DIR <- '.data/power_curves/'
PLOTS_DIR <- file.path(RESULTS_DIR, 'plots')
OUT_DIR <- file.path(RESULTS_DIR, 'out')

dir.create(PLOTS_DIR, showWarnings = F, recursive = T)
dir.create(OUT_DIR, showWarnings = F, recursive = T)
  
library(ggplot2)
library(tidyr)
library(dplyr)
# lbl <- 'logitG_1500_v1'
# lbl <- 'liabilityG_v3_stitched'
# lbl <- 'logitG_1024_beta0.5_optim_v1'
# lbl <- 'liabilityG_1024_beta0.5_optim_v1'
lbl <- 'logitG_low_beta_test_4'

lrt.result.dir <- file.path('.data/results/', lbl)

lrt.result <- lapply(
    dir(lrt.result.dir),
    function(f) { readRDS(file.path(lrt.result.dir, f))
  }) %>%
  bind_rows() %>%
  arrange(gamma, eta)

models.to.plot <- c('ll.cc', 'll.co', 'rev.co', 'fwd')
# model.lbls <- c(
#   'll.co' = 'Rev. Cond. Cases Only',
#   'll.cc' = 'Rev. Cond. Cases+Controls',
#   'rev.co' = 'Rev. Cases Only',
#   'fwd' = 'Fwd.'
# )

power.df <- lrt.result %>%
  select(gamma, eta, ends_with('p.val')) %>%
  pivot_longer(cols = -c('gamma', 'eta'), names_to = 'model', values_to = 'p.val') %>%
  group_by(gamma, eta, model) %>%
  mutate(p.val = ifelse(is.na(p.val), 1, p.val)) %>%
  summarize(power = mean(p.val < 0.05)) %>%
  mutate(model = gsub('.p.val', '', model)) %>%
  ungroup() %>%
  arrange(gamma, eta)

power.df %>%
  mutate(eta = as.factor(eta)) %>%
  ggplot(aes(x = gamma, y = power, colour = model, shape = eta)) +
  geom_point()

sigmoid <- function(gamma, a, b, c) {
  1 / (1 + exp(-(a * gamma + b))) + c
}

gamma.vals <- seq(0, 1, 0.01)
power.df.smooth <- data.frame()
for (model in models.to.plot) {
  for (eta in unique(power.df$eta)) {
    print(paste('fitting sigmoid for', model, 'eta=', eta))
    a.df <- data.frame(
      gamma = gamma.vals,
      eta = eta,
      model = model
    )
    if (model == 'fwd.p.val') {
      print('dbg')
    }
    
    # if (a[a$gamma == 0,]$power > a[a$gamma == 1,]$power) {
    #   # nls.fit <- nls(
    #   #   power ~ flipped.sigmoid(gamma, a, b, c),
    #   #   data = a,
    #   #   start = list(a = 10, b = -1, c = 0)
    #   # )
    #   next
    # }
    
    try({
      a <- power.df[power.df$model == model & power.df$eta == eta,]
      nls.fit <- nls(
        power ~ sigmoid(gamma, a, b, c),
        data = a,
        start = list(a = 10, b = -1, c = 0)
      )
      
      a.df$power <- predict(nls.fit, newdata = a.df)
      
      power.df.smooth <- bind_rows(power.df.smooth, a.df)
    })
  }
}

power.df.smooth

power.df$compare <- 'og'
power.df.smooth$compare <- 'smooth'

compare.df <- bind_rows(
  power.df %>% inner_join(select(power.df.smooth, gamma, eta, model), by = c('gamma', 'eta', 'model')),
  power.df.smooth
) %>%
  mutate(eta = as.factor(eta))
  # mutate(model = model.lbls[model])


# model.colors <- setNames(scales::hue_pal()(4), c('fwd', 'll.cc', 'll.co', 'rev.co'))
model.colors <- c(
  'fwd' = '#f42e3d',
  'll.cc' = '#2ff4e4',
  'll.co' = '#2ff481',
  'rev.co' = '#e32ff4'
)

power.curve <- compare.df %>%
  ggplot(aes(x = gamma, y = power, color = model)) +
  scale_color_manual(values = model.colors) +
  geom_line(aes(linetype = eta, linewidth = interaction(model, eta)), data = subset(compare.df, compare == 'smooth')) +
  geom_point(aes(shape = eta, size = interaction(model, eta)), data = subset(compare.df, compare == 'og')) +
  scale_shape_manual(values = c('0' = 20, '0.1' = 21)) +
  # scale_shape_manual(values = c(
  #   'fwd.0' = 16,
  #   'rev.co.0' = 16,
  #   'll.cc.0' = 16,
  #   'll.co.0' = 16,
  #   'fwd.0.1' = 2,
  #   'rev.co.0.1' = 2,
  #   'll.cc.0.1' = 2/3,
  #   'll.co.0.1' = 2/3
  # )) +
  # scale_size_manual(values = c('0' = 3, '0.1' = 2)) +
  scale_size_manual(values = c(
    'fwd.0' = 5,
    'rev.co.0' = 5,
    'll.cc.0' = 5/2,
    'll.co.0' = 5/2,
    'fwd.0.1' = 3,
    'rev.co.0.1' = 3,
    'll.cc.0.1' = 3/2,
    'll.co.0.1' = 3/2
  )) +
  scale_linetype_manual(values = c('0' = 'solid', '0.1' = 'dotted')) +
  scale_linewidth_manual(values = c(
    'fwd.0' = 2,
    'rev.co.0' = 2,
    'll.cc.0' = 2/3,
    'll.co.0' = 2/3,
    'fwd.0.1' = 3,
    'rev.co.0.1' = 3,
    'll.cc.0.1' = 1,
    'll.co.0.1' = 1
  )) +
  theme_bw() +
  theme(
    # strip.text = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 12),
    # legend.title = element_text(face = "bold", size = 11),
    # legend.text = element_text(size = 11),
    # legend.position = "none"
  ) +
  labs(
    x = expression(gamma),
    y = 'Power',
    color = 'Model',
    shape = expression(eta),
    size = expression(eta),
    linetype = expression(eta),
  )

power.curve

saveRDS(lrt.result, file = file.path(OUT_DIR, paste0(lbl, '.lrt.result.rds')))
saveRDS(power.df, file = file.path(OUT_DIR, paste0(lbl, '.power.rds')))

save.plot(
  paste('power', lbl, sep='.'),
  my.plot = power.curve,
  w = 6,
  h = 4.5,
  out.dir = PLOTS_DIR
)

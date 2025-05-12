library(data.table)
library(lubridate)
library(survival)
pd.age.df <- fread(file.path(SRC_DATA_DIR, "cohort.meta.tsv"), sep="\t", header=T) %>%
  mutate(
    sample.id = paste(`Participant ID`, `Participant ID`, sep='_'),
  ) %>%
  select(
    sample.id,
    yob = `Year of birth`,
    pd.rep.date = `Date G20 first reported (parkinson's disease)`
  ) %>%
  filter(
    !is.na(yob)
  ) %>% mutate(
    pd.rep.age = as.integer(format(pd.rep.date, "%Y")) - yob
  )
  
pd.age.df

npc2.id <- '14:74484507:C:T'
gba.id <- '1:155236246:G:A'
arsa.id <- '22:50625988:T:C'

npc2.gt <- geno.mtx[
  match(cohort.dfs$PD$sample.id, rownames(geno.mtx)),
  which(colnames(geno.mtx) == npc2.id)
]

gba.gt <- geno.mtx[
  match(cohort.dfs$PD$sample.id, rownames(geno.mtx)),
  which(colnames(geno.mtx) == gba.id)
]

arsa.gt <- geno.mtx[
  match(cohort.dfs$PD$sample.id, rownames(geno.mtx)),
  which(colnames(geno.mtx) == arsa.id)
]

thingy <- cohort.dfs$PD%>%
  bind_cols(
    npc2 = npc2.gt > 0,
    gba = gba.gt > 0,
    arsa = arsa.gt > 0
  ) %>%
  left_join(pd.age.df, by = 'sample.id')

thingy %>%
  select(
    case,
    gba.clinvar = rv,
    gba,
    npc2,
    arsa,
    pd.onset.age = pd.rep.age,
    prs
  ) %>%
  saveRDS(file.path(DATA_DIR, 'pd.age.variant.df.rds'))

# thingy <- cohort.dfs$PD%>%
#   left_join(pd.age.df, by = 'sample.id')

thingy.npc2.med <- thingy %>%
  filter(case) %>%
  group_by(npc2) %>%
  summarize(med = median(pd.rep.age))

thingy.npc2.med

thingy %>%
  filter(case) %>%
  ggplot(aes(x = pd.rep.age, color = npc2)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept = med, color = npc2), data = thingy.npc2.med)

thingy %>%
  filter(!case & !is.na(yob)) %>%
  mutate(age = 2025 - yob) %>%
  select(case, npc2, yob, age) %>%
  mutate(age.percentile = ecdf(age)(age)) %>%
  saveRDS(file.path(DATA_DIR, 'pd-controls.npc2.age.percentiles.df.rds'))

# table(thingy$case, thingy$rv)

# broom::tidy(glm(rv ~ pd.rep.age, data = thingy))

sd <- survdiff(Surv(pd.rep.age, case) ~ arsa, data = thingy %>% filter(case))
sd
sf <- survfit(Surv(pd.rep.age, case) ~ arsa, data = thingy %>% filter(case))
plot(
  sf,
  col = c("blue", "red"),
  lty = 1,
  xlab = "PD Onset Age",
  ylab = "P(Undiagnosed)",
  main = "P(Undiagnosed) by Onset Age - ARSA"
)

legend(
  "topright",
  legend = c("RV-", "RV+"),
  col = c("blue", "red"),
  lty = 1,
  bty = "n"
)

thingy.case <- thingy[thingy$case,]
t.test(thingy.case[thingy.case$rv == 1,]$prs, thingy.case[thingy.case$rv == 0,]$prs)
wilcox.test(thingy.case[thingy.case$rv == 1,]$prs, thingy.case[thingy.case$rv == 0,]$prs)
ks.test(thingy.case[thingy.case$rv == 1,]$prs, thingy.case[thingy.case$rv == 0,]$prs)

thingy.case %>%
  ggplot(aes(x = factor(rv), y = prs)) +
  geom_boxplot()


annotations %>%
  # filter(grepl('c.*96A>G', cchange)) %>%
  filter(gene == 'ARSA') %>%
  select(marker.id, gene, mutation.type, cchange, achange) %>%
  arrange(marker.id) %>%
  View()
  

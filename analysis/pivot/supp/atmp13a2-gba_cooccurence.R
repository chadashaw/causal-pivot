# GBA and ATP13A2
# co-occurence - 
# case-control regression
# PD ~ ATP13A2 * GBA 
# CADD

# find person w/ this variant 1:16997107:G:A
# find other variants in codon
annotations %>%
  filter(gene == 'ATP13A2') %>%
  filter(grepl('370', achange)) %>%
  # filter(marker.id == '1:16997107:G:A') %>%
  select(marker.id, gene, mutation.type, cchange, achange)

a <- geno.mtx[,c('1:16997106:C:T', '1:16997107:G:A')]
has.markers = a[rowSums(a) > 0,]
has.markers  

cohort.meta %>%
  # filter(!is.na(pd.rep.date)) %>%
  filter(sample.id %in% rownames(has.markers)) %>%
  select(sample.id, age, sex, pd.rep.date) %>%
  pull(age) %>%
  sd

has.markers

cohort.dfs$PD %>%
  mutate(p = ecdf(prs)(prs)) %>%
  filter(sample.id %in% rownames(has.markers))

cv.path <- annotations %>%
  filter(gene %in% c('GBA','ATP13A2')) %>%
  mutate(
    class.rare = gnomad3.af_nfe < gnomad.cutoff
  ) %>%
  classify.clinvar() %>%
  classify.lof() %>%
  mutate(class.positive = class.clinvar.pathogenic | (class.rare & class.loss.of.function)) %>%
  filter((cadd_exome.phred > 25 & revel.score > .9) | class.positive) %>%
  select(marker.id, gene, gnomad3.af_nfe)

cv.path %>%
  filter(gene == 'ATP13A2')

cv.path %>%
  group_by(gene) %>%
  count()

my.data <- alt.sums.by.var(
  geno.mtx[
    match(cohort.dfs$PD$sample.id, rownames(geno.mtx)),
    match(cv.path$marker.id, colnames(geno.mtx))
  ],
  tibble::deframe(cv.path)
) %>%
  as.matrix() %>%
  as.data.frame()

pooh.bear <- cbind(my.data, cohort.dfs$PD[match(rownames(my.data), cohort.dfs$PD$sample.id),])

broom::tidy(glm(case ~ ATP13A2 * GBA, family = binomial(link = "logit"), data = pooh.bear))

broom::tidy(
  glm(ATP13A2 ~ GBA, family = binomial(link = "logit"), data = pooh.bear[pooh.bear$case,])
)

table(piglet$GBA, piglet$ATP13A2)

piglet <- pooh.bear[pooh.bear$case,]
broom::tidy(fisher.test(table(piglet$GBA, piglet$ATP13A2)))

pooh.bear[pooh.bear$case & pooh.bear$GBA == 1,]
    
annotations %>%
  filter(gene == 'ATP13A2') %>%
  left_join(rv.flags, by = 'marker.id') %>%
  filter(class.positive)

median(na.omit(annotations$revel.score))

tigger <- pooh.bear %>%
  left_join(pd.age.df, by = 'sample.id') %>%
  filter(!is.na(pd.rep.age)) %>%
  mutate(
    ATP13A2 = ATP13A2 > 0,
    GBA = GBA  > 0,
  )

# age distribution of ATP13A2, GBA, both
pooh.bear %>%
  left_join(pd.age.df, by = 'sample.id') %>%
  filter(!is.na(pd.rep.age)) %>%
  mutate(
    ATP13A2 = ATP13A2 == 1,
    GBA = GBA == 1,
    both = ATP13A2 & GBA
  ) %>%
  select(sample.id, case, ATP13A2, GBA, both, pd.rep.age) %>%
  pivot_longer(cols = c('ATP13A2', 'GBA', 'both'), names_to = 'gene', values_to = 'rv') %>%
  filter(rv) %>%
  ggplot(aes(x = pd.rep.age, color = gene)) +
  geom_density(alpha = 0.3)


sd <- survdiff(Surv(pd.rep.age, case) ~ ATP13A2, data = tigger %>% filter(case))
sd
sf <- survfit(Surv(pd.rep.age, case) ~ ATP13A2, data = tigger %>% filter(case))
plot(
  sf,
  col = c("blue", "red"),
  lty = 1,
  xlab = "PD Onset Age",
  ylab = "P(Undiagnosed)",
  main = "P(Undiagnosed) by Onset Age - ATP13A2"
)

legend(
  "topright",
  legend = c("RV-", "RV+"),
  col = c("blue", "red"),
  lty = 1,
  bty = "n"
)

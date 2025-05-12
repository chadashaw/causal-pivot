out.dir.ukb <- "./.data/results/pivot-ukb/out"
out.dir.big3 <- "./.data/results/pivot-big3/out"

logitG.mle.result.ukb <- readRDS(file.path(out.dir.ukb, 'logitG.mle.result.rds'))
logitG_A.mle.result.ukb <- readRDS(file.path(out.dir.ukb, 'logitG_A.mle.result.rds'))
# plot.mle.perm.contour(logitG.mle.result.ukb)
# plot.mle.perm.hist(logitG.mle.result.ukb)

logitG.mle.result.big3 <- readRDS(file.path(out.dir.big3, 'logitG.mle.result.rds'))
logitG_A.mle.result.big3 <- readRDS(file.path(out.dir.big3, 'logitG_A.mle.result.rds'))
# plot.mle.perm.contour(logitG.mle.result.big3)
# plot.mle.perm.hist(logitG.mle.result.big3)

target.transform <- function(mle.target) {
  mle.target %>%
    filter(term %in% c('G', 'G:X')) %>%
    select(cohort.name, gene.grp, term, estimate) %>%
    pivot_wider(id_cols = c('cohort.name', 'gene.grp'), names_from = 'term', values_from = 'estimate') %>%
    select(
      cohort.name,
      gene.grp,
      gamma = G,
      eta = `G:X`,
    )
}

do.the.thing <- function(mle.result) {
  inner_join(
    mle.result$conf.int,
    target.transform(mle.result$mle.target),
    by = c('cohort.name', 'gene.grp'),
    suffix = c('.est', '.point.est')
  ) %>%
    select(cohort.name, gene.grp, starts_with('gamma'), starts_with('eta'))
}

prs.compare.list <- list(
  conf.int.logitG.ukb = do.the.thing(logitG.mle.result.ukb),
  conf.int.logitG.big3 = do.the.thing(logitG.mle.result.big3),
  conf.int.logitG_A.ukb = do.the.thing(logitG_A.mle.result.ukb),
  conf.int.logitG_A.big3 = do.the.thing(logitG_A.mle.result.big3)
)

prs.compare.list$conf.int.cmp.logitG <- inner_join(
  prs.compare.list$conf.int.logitG.ukb,
  prs.compare.list$conf.int.logitG.big3,
  by = c('cohort.name', 'gene.grp'),
  suffix = c('.ukb', '.big3')
)

prs.compare.list$conf.int.cmp.logitG_A <- inner_join(
  prs.compare.list$conf.int.logitG_A.ukb,
  prs.compare.list$conf.int.logitG_A.big3,
  by = c('cohort.name', 'gene.grp'),
  suffix = c('.ukb', '.big3')
)

wb <- openxlsx::createWorkbook()

for (name in names(prs.compare.list)) {
  openxlsx::addWorksheet(wb, name)
  openxlsx::writeData(wb, name, prs.compare.list[[name]])
}

openxlsx::saveWorkbook(wb, file.path("./.data/results/", 'prs.compare.xlsx'), overwrite = T)
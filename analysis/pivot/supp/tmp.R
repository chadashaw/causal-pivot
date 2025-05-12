as_tibble(a) %>%
  select(LDLR.prev, LDLR.new) %>%
  filter(!LDLR.prev & LDLR.new)
# 3 new
as_tibble(a) %>%
  select(BRCA1.prev, BRCA1.new) %>%
  filter(!BRCA1.prev & BRCA1.new)
# 8 new
as_tibble(a) %>%
  select(GBA.prev, GBA.new) %>%
  filter(!GBA.prev & GBA.new)
# 10 new

rv.prs.df.prev <- bind_cols(
  cohort.flags,
  events.df.prev[match(rownames(cohort.flags), rownames(events.df.prev)),]
) %>%
  mutate(sample.id = rownames(.)) %>%
  inner_join(prs, by = 'sample.id') %>%
  select(sample.id, everything()) %>%
  as_tibble()

rv.prs.df.new <- bind_cols(
  cohort.flags,
  events.df.new[match(rownames(cohort.flags), rownames(events.df.new)),]
) %>%
  mutate(sample.id = rownames(.)) %>%
  inner_join(prs, by = 'sample.id') %>%
  select(sample.id, everything()) %>%
  as_tibble()

b <- full_join(
  rv.prs.df.prev,
  rv.prs.df.new %>% select(sample.id, LDLR, BRCA1, GBA),
  by = 'sample.id', suffix = c('.prev', '.new'))

b %>%
  select(sample.id, HC190, LDLR.prev, LDLR.new) %>%
  pivot_longer(cols = starts_with('LDLR')) %>%
  group_by(HC190, name, value) %>% count

b %>%
  select(sample.id, BC, BRCA1.prev, BRCA1.new) %>%
  pivot_longer(cols = starts_with('BRCA1')) %>%
  group_by(BC, name, value) %>% count

b %>%
  select(sample.id, PD, GBA.prev, GBA.new) %>%
  pivot_longer(cols = starts_with('GBA')) %>%
  group_by(PD, name, value) %>% count

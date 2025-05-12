# define "rare' variant cutoff - 0.1% allele frequency
gnomad.cutoff <- 0.001

class.na.false <- function(rv.flags) {
  rv.flags %>%
  rowwise() %>%
  mutate(across(.cols = starts_with('class.'), .fns = ~ifelse(is.na(.), FALSE, .))) %>%
  ungroup()
}

classify.clinvar <- function(annotations) {
  annotations %>%
    mutate(
      clinvar.pathogenic = grepl("pathogenic", clinvar.sig, ignore.case=T),
      clinvar.conflicting = grepl("conflicting", clinvar.sig, ignore.case=T),
      clinvar.qualifier = grepl("\\|", clinvar.sig, ignore.case=T),
      clinvar.sig_conf.pathogenic = grepl("pathogenic", clinvar.sig_conf, ignore.case = T),
      clinvar.sig_conf.benign = grepl("benign", clinvar.sig_conf, ignore.case = T),
      class.clinvar.pathogenic = (clinvar.pathogenic & !clinvar.qualifier & !clinvar.conflicting) | (clinvar.sig_conf.pathogenic & !clinvar.sig_conf.benign),
    ) %>%
      select(
        -clinvar.pathogenic,
        -clinvar.conflicting,
        -clinvar.qualifier,
        -clinvar.sig_conf.pathogenic,
        -clinvar.sig_conf.benign
      )
}

classify.lof <- function(annotations) {
  annotations %>%
    mutate(
      class.loss.of.function = grepl('stop_gained|frameshift_truncation', mutation.type, ignore.case=T),
    )
}

classify.synonymous <- function(annotations) {
  annotations %>%
    mutate(
      class.synonymous = grepl('synonymous_variant', mutation.type, ignore.case=T),
    )
}

classify.missense <- function(annotations) {
  annotations %>%
    mutate(
      class.missense = grepl('missense_variant', mutation.type, ignore.case=T),
    )
}

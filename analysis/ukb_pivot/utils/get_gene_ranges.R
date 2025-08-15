library(biomaRt)
mart <- useEnsembl(biomart = "genes")
ensembl <- useDataset(dataset = "hsapiens_gene_ensembl", mart = mart)

attributes <- c("chromosome_name", "start_position", "end_position", "transcript_start", "transcript_end", "ensembl_gene_id", 
                "ensembl_transcript_id", "external_gene_name", "transcript_is_canonical")
filters <- "external_gene_name"
values <- "GBA1"

data <- getBM(
  attributes = attributes,
  filters = filters,
  values = values,
  mart = ensembl
)

chroms <- c(as.character(seq(23)), c('X', 'Y', 'MT'))

tx <- data[
  !is.na(data$transcript_is_canonical)
  & data$transcript_is_canonical == T
  & data$chromosome_name %in% chroms,
]
tx

my.range <- list(
  'chr' = tx$chromosome_name,
  'from' = tx$start_position,
  'to' = tx$end_position
)

values
my.range
my.range$to - my.range$from


library(data.table, quietly = T, warn.conflicts = F)
library(dplyr, quietly = T, warn.conflicts = F)
library(tidyr, quietly = T, warn.conflicts = F)
library(purrr, quietly = T, warn.conflicts = F)
library(Matrix, quietly = T, warn.conflicts = F)
source("utils/data.R")

# if SRC_DATA_DIR is undefined assume CWD
if (!exists("SRC_DATA_DIR")) {
  SRC_DATA_DIR <- getwd()
}

# if DATA_DIR is undefined assume CWD
if (!exists("DATA_DIR")) {
  DATA_DIR <- getwd()
}

# Reads alt counts - aka genotypes - matrix from data source directory ####
#
# rows -> samples - plink2 IDs - EID_EID - e.g. (000001_000001)
# cols -> variants - IDs from UKB RAP BGEN - c:s:r:a - e.g. 1:10000:A:AG
#
# Assumes source directory contains necessary files including:
#  - geno.mtx - genotypes encoded in Matrix Market Coordinate Format
#  - sample.ids - line-separated list of sample IDs
#  - marker.ids - line-separated list of marker IDs
#  
# The length of the sample.ids and marker.ids files must respectively match
# the number of rows and columns in the genotype matrix.
# 
# Note: on our systems the resulting dataframe can take:
#  - ~10GB in memory
#  - ~5 minutes to read
#
# see https://math.nist.gov/MatrixMarket/formats.html
print("Reading genotypes matrix...")
sample.ids <- readLines(file.path(SRC_DATA_DIR, "sample.ids"))
marker.ids <- readLines(file.path(SRC_DATA_DIR, 'marker.ids')) %>%
  convert.marker.ids
geno.mtx <- as(readMM(file.path(SRC_DATA_DIR, 'geno.mtx')), "CsparseMatrix") # this can take a couple of minutes
rownames(geno.mtx) <- sample.ids
colnames(geno.mtx) <- marker.ids

# remove markers with no calls
geno.mtx <- geno.mtx[,which(Matrix::colSums(geno.mtx) > 0)]; gc();
dim(geno.mtx)

# Read cohort metadata ####
#
# rows -> samples
# cols -> metadata (e.g. Age, Sex, PCA components, PRS scores)
#
# Sample IDs reformated to match plink IDs - 000001 -> 000001_000001
# Genetic sex decoded 0 -> F, 1 -> M
# 
# Note: we don't have measurements (genotypes) for all samples in cohort
# dataframe re-ordered & filtered to samples w/ genotypes
print("Reading cohort metadata...")
cohort.meta <- fread(file.path(SRC_DATA_DIR, "cohort.meta.tsv"), sep="\t", header=T)  %>%
  mutate(
    sample.id = paste(`Participant ID`, `Participant ID`, sep='_'),
    age = as.integer(format(Sys.Date(), "%Y")) - `Year of birth`,
    sex = ifelse(`Genetic sex` == 'Female', 'F', 'M'),
    bc.inst.0 = grepl('^C50', `Type of cancer: ICD10 | Instance 0`),
    bc.inst.1 = grepl('^C50', `Type of cancer: ICD10 | Instance 1`)
  ) %>%
  as_tibble %>%
  select(
    sample.id,
    age,
    sex,
    bc.inst.0,
    bc.inst.1,
    ldl.inst.0 = `LDL direct | Instance 0`,
    pd.rep.date = `Date G20 first reported (parkinson's disease)`,
    # pc.1 = `Genetic principal components | Array 1`,
    # pc.2 = `Genetic principal components | Array 2`,
    # pc.3 = `Genetic principal components | Array 3`,
    # pc.4 = `Genetic principal components | Array 4`,
    # pc.5 = `Genetic principal components | Array 5`,
    starts_with('Genetic principal components | Array'),
    starts_with('Standard PRS'),
  ) %>%
  rename_with(~gsub("Genetic principal components \\| Array ", "pc.", .), starts_with("Genetic principal components | Array "))

# include only samples w/ cohort metadata and genotypes
sample.ids <- intersect(cohort.meta$sample.id, rownames(geno.mtx))
cohort.meta <- cohort.meta[na.omit(match(sample.ids, cohort.meta$sample.id)),]
geno.mtx <- geno.mtx[na.omit(match(rownames(geno.mtx), sample.ids)),]

# Read variant annotations from open cravat ####
#
# Open Cravat TSV report is post processed to remove \^#\ headers and gzip
# Gzipped TSV is read and subsetted and renamed:
# - columns are renamed for legibility (e.g. "ref_base" -> "ref")
print("Reading variant annotations...")
annotations <- fread(file.path(SRC_DATA_DIR, 'markers.annotations.tsv.gz'), sep='\t') %>%
  as_tibble %>%
  select(
    -uid
  ) %>%
  rename(
    ref = ref_base,
    alt = alt_base,
    gene = hugo,
    mutation.type = so,
    num.samples = numsample,
  ) %>%
  mutate(marker.id = paste(gsub('chr', '', chrom), pos, ref, alt, sep=':')) %>%
  .[-which(duplicated(.$marker.id)),] %>% # TODO: figure out duplicates - only 3
  select(
    marker.id, chrom, pos, ref, alt,
    coding, gene, transcript, mutation.type, cchange, achange,
    tcg_clinvar.allele_id,
    starts_with('tcg_clinvar.sig'),
    gnomad3.af_nfe,
    revel.score,
    cadd_exome.phred,
    starts_with('am_esm1b'),
  ) %>%
  .[na.omit(match(colnames(geno.mtx), .$marker.id)),] 
# we switched to using our manually loaded clinvar annotator
colnames(annotations) <- gsub("^tcg_", "", colnames(annotations))

print("Saving intermediates...")
saveRDS(cohort.meta, file.path(DATA_DIR, 'cohort.meta.rds'))
saveRDS(annotations, file.path(DATA_DIR, 'annotations.rds'))
saveRDS(geno.mtx, file.path(DATA_DIR, 'geno.mtx.rds'))
print(paste("Objects saved to: ", DATA_DIR))

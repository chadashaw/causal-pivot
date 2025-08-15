library(dplyr, quietly = T, warn.conflicts = F)
library(ggplot2, quietly = T, warn.conflicts = F)
library(purrr, quietly = T, warn.conflicts = F)
library(tidyr, quietly = T, warn.conflicts = F)
library(Matrix, quietly = T, warn.conflicts = F)

# font stuff
library(showtext)
library(sysfonts)
font_add(
  family = "Arial",
  regular = "Arial.ttf",
  bold = "Arial_Bold.ttf",
  italic = "Arial_Italic.ttf",
  bolditalic = "Arial_Bold_Italic.ttf"
)
showtext_auto()
cowplot::set_null_device("png")
# end font stuff

# conflicted
library(conflicted, quietly = T, warn.conflicts = F)
conflict_prefer("select", "dplyr", quiet = T)
conflict_prefer("filter", "dplyr", quiet = T)
options(dplyr.summarise.inform = F)
# end conflicted

n.mle.perms <- 64
PRS_SOURCE <- NULL
# PRS_SOURCE <- "./.data/pgs/custom.scores.txt.gz"
RESULTS_DIR <- "./.data/results/pivot"

# set source and result data dirs from environment ####
SRC_DATA_DIR <- Sys.getenv("SRC_DATA_DIR")

# set and create results directories ####

OUT_DIR <- file.path(RESULTS_DIR, 'out') # for primary raw data results
PLOTS_DIR <- file.path(RESULTS_DIR, 'plots') # for plots
DATA_DIR <- file.path(RESULTS_DIR, 'data') # for intermediates

dir.create(DATA_DIR, showWarnings = F, recursive = T)
dir.create(PLOTS_DIR, showWarnings = F, recursive = T)
dir.create(OUT_DIR, showWarnings = F, recursive = T)

output_dir <- file.path(RESULTS_DIR, "knits")
intermediates_dir <- file.path(RESULTS_DIR, "knits")
knit_root_dir <- file.path(RESULTS_DIR, "knits")

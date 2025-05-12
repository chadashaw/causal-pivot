library(dplyr, quietly = T, warn.conflicts = F)
library(ggplot2, quietly = T, warn.conflicts = F)
library(purrr, quietly = T, warn.conflicts = F)
library(tidyr, quietly = T, warn.conflicts = F)
library(Matrix, quietly = T, warn.conflicts = F)

# font stuff
library(showtext)
library(sysfonts)
font_add(family = "Arial", regular = "Arial.ttf", bold = "Arial_Bold.ttf")
showtext_auto()
cowplot::set_null_device("png")
# end font stuff

# conflicted
library(conflicted, quietly = T, warn.conflicts = F)
conflict_prefer("select", "dplyr", quiet = T)
conflict_prefer("filter", "dplyr", quiet = T)
options(dplyr.summarise.inform = F)
# end conflicted

# library(randomForest, quietly = T, warn.conflicts = F)
# library(caret, quietly = T, warn.conflicts = F)
# library(patchwork, quietly = T, warn.conflicts = F)
# library(lmtest, quietly = T, warn.conflicts = F)
# library(distances, quietly = T, warn.conflicts = F)
# library(Rtsne, quietly = T, warn.conflicts = F)
# library(broom, quietly = T, warn.conflicts = F)
# source("utils/main.R")
# source("utils/data.R")
# source("utils/variant.classifiers.R")
# source("utils/lrt.R")

# dev - ukb
# n.mle.perms <- 16
# PRS_SOURCE <- NULL
# RESULTS_DIR <- "./.data/ukb-dev/"

# dev - big3
# n.mle.perms <- 16
# PRS_SOURCE <- "./.data/pgs/big3.scores.txt.gz"
# RESULTS_DIR <- "./.data/.results/big3-dev/"

# prod - ukb
n.mle.perms <- 512
PRS_SOURCE <- NULL
RESULTS_DIR <- "./.data/results/pivot-ukb"

# prod - big3
# n.mle.perms <- 8
# PRS_SOURCE <- "./.data/pgs/big3.scores.txt.gz"
# RESULTS_DIR <- "./.data/results/pivot-big3/"

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

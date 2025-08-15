# # data manipulation
# library(dplyr, quietly = T, warn.conflicts = F)
# library(purrr, quietly = T, warn.conflicts = F)
# library(tidyr, quietly = T, warn.conflicts = F)
# library(Matrix, quietly = T, warn.conflicts = F)
# library(openxlsx, quietly = T, warn.conflicts = F)
# 
# # plotting
# library(ggplot2, quietly = T, warn.conflicts = F)
# library(paletteer, quietly = T, warn.conflicts = F)
# library(waffle, quietly = T, warn.conflicts = F)
# library(patchwork, quietly = T, warn.conflicts = F)
# 
# # modeling & testing
# library(lmtest, quietly = T, warn.conflicts = F)
# library(caret, quietly = T, warn.conflicts = F)
# library(distances, quietly = T, warn.conflicts = F)
# library(randomForest, quietly = T, warn.conflicts = F)
# library(Rtsne, quietly = T, warn.conflicts = F)
# 
# # custom utils
# source("utils/main.R")
# source("utils/data.R")
# source("utils/variant.classifiers.R")
# source('utils/lrt.R')

rmarkdown::render("setup.Rmd", envir = globalenv(), output_dir = output_dir, intermediates_dir = intermediates_dir)
rmarkdown::render("rv.summaries.Rmd", envir = globalenv(), output_dir = output_dir, intermediates_dir = intermediates_dir)
rmarkdown::render("lrt.Rmd", envir = globalenv(), output_dir = output_dir, intermediates_dir = intermediates_dir)
rmarkdown::render("liabilityG.lrt.Rmd", envir = globalenv(), output_dir = output_dir, intermediates_dir = intermediates_dir)
rmarkdown::render("collider.plots.Rmd", envir = globalenv(), output_dir = output_dir, intermediates_dir = intermediates_dir)
rmarkdown::render("ancestry.control.Rmd", envir = globalenv(), output_dir = output_dir, intermediates_dir = intermediates_dir)




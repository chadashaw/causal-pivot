source("./renv/activate.R")

library(dplyr, quietly = T, warn.conflicts = F)
library(tidyr, quietly = T, warn.conflicts = F)
library(parallel, quietly = T, warn.conflicts = F)
library(lmtest, quietly = T, warn.conflicts = F)

source('utils.R')

args <- commandArgs(trailingOnly = TRUE)
n.sims <- as.numeric(args[1])

LRT_EQNS_DIR <- './lrt_equations/'

set.seed(
  as.integer(
    abs(
      Reduce(`+`, utf8ToInt(Sys.info()[["nodename"]]))) +
      Sys.getpid() +
      as.numeric(Sys.time())
    )
  )

source('sim.R')
result$run.id <- Sys.getenv("run_id")
result$job.id <- Sys.getenv("DX_JOB_ID")
saveRDS(result, 'result.rds')
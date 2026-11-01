# CLI: extract the PEST posterior for a lake from its stage-4 checkpoint.
#   Rscript R/pest_workflow/extract_posterior.R <LAKE_TAG> [CHECKPOINT_ROOT]
# Writes pest_workflow/<lake>/posterior/ (see pest_extract.R).
suppressMessages({ library(AEME); library(aemetools); library(dplyr) })
source("R/pest_workflow/manifest.R")
source("R/pest_workflow/setup.R")
source("R/pest_workflow/pest_extract.R")
args <- commandArgs(trailingOnly = TRUE)
d <- if (length(args) >= 2) extract_posterior(args[1], checkpoint_root = args[2]) else extract_posterior(args[1])
m <- read_manifest(d)
cat(sprintf("POSTERIOR %s: cal_id %s, %d members, %d varying params\n", args[1], m$cal_id, m$n_members, m$n_params_varying))

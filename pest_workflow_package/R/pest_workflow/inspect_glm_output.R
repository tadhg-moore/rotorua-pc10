# Open the raw GLM netCDF of a forward run with the posterior best parameters and summarise the solids-related
# variables, to see what the modelled NCS_ss1 series really is (used for Spectacle).
#   Rscript R/pest_workflow/inspect_glm_output.R <LAKE_TAG>
suppressMessages({ library(targets); library(AEME); library(aemetools); library(dplyr) })
source("R/pest_workflow/manifest.R"); source("R/pest_workflow/setup.R"); source("R/pest_workflow/pest_extract.R")
source("R/set_lake_inflows.R"); source("R/lspc_to_aeme.R"); source("R/add_block_spinup.R")
source("R/pest_workflow/run_ensemble.R")
tag <- commandArgs(trailingOnly = TRUE)[1]
ROOT <<- file.path(LOCAL_ROOT, "_builds", paste0("insp_", tag)); dir.create(ROOT, recursive = TRUE, showWarnings = FALSE)
aeme0 <<- build_ensemble_aeme0(tag, "baseline")
pp <- read_posterior_ptabs(tag)
ptab <- pp$best[, intersect(PARAM_COLS, names(pp$best))]; for (cc in setdiff(PARAM_COLS, names(ptab))) ptab[[cc]] <- NA
ptab <- ptab[, PARAM_COLS]
if (nzchar(Sys.getenv("INSPECT_DROP_NC"))) { ptab <- ptab[!grepl("noncohesive", ptab$name), ]; cat("noncohesive rows dropped: file/object defaults in use
") }
if (nzchar(Sys.getenv("INSPECT_EPS0"))) { ptab$value[ptab$name == "aed_noncohesive/epsilon"] <- 0; cat("epsilon set to 0
") }
b <- fresh_build("insp", VARS_SIM, use_bgc = TRUE, switches = SWITCHES, n_zones = n_glm_zones)
a <- run_aeme_param(aeme = b$aeme, param = ptab, model = MODEL, path = b$path, return_aeme = TRUE)
ncs <- list.files(ROOT, pattern = "[.]nc$", recursive = TRUE, full.names = TRUE); cat("netCDF files:", ncs, sep = "\n  ")
nc <- ncdf4::nc_open(ncs[which.max(file.size(ncs))])
vn <- names(nc$var); hit <- grep("ss|NCS|TSS|tss|solid|sed", vn, value = TRUE, ignore.case = TRUE)
cat("\nsolids/sediment-related variables in the output:\n"); print(hit)
for (v in hit) { x <- tryCatch(ncdf4::ncvar_get(nc, v), error = function(e) NULL); if (!is.null(x)) cat(sprintf("%-28s dims %-14s mean %10.4g  min %10.4g  max %10.4g  units %s\n", v, paste(dim(x), collapse = "x"), mean(x, na.rm = TRUE), min(x, na.rm = TRUE), max(x, na.rm = TRUE), nc$var[[v]]$units)) }
for (v in c("temp", "V", "lake_level", "NCS_ss1", "NCS_ss2")) if (v %in% vn) { x <- ncdf4::ncvar_get(nc, v); cat(sprintf("%-10s mean %10.4g min %10.4g max %10.4g\n", v, mean(x, na.rm = TRUE), min(x, na.rm = TRUE), max(x, na.rm = TRUE))) }
ncdf4::nc_close(nc)
cat("\nAEME key_naming for NCS_ss1:\n"); print(AEME::key_naming[AEME::key_naming$var_aeme == "NCS_ss1", ])
unlink(ROOT, recursive = TRUE, force = TRUE)

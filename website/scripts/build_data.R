Sys.setenv(RENV_PROJECT = normalizePath(".."))
source("../renv/activate.R")

AEME::install_glm_aed()

targets::tar_make()

suppressPackageStartupMessages(library(AEME)); ns <- asNamespace("AEME")
for (f in c("tli_annual", "tli_annual_epi", "tli_year", "calc_tli_secchi", "calc_tli3", "calc_tli4")) { cat("=====", f, "\n"); print(get(f, ns)) }
print(get("tli_min_obs_per_year", ns)); print(get("tli_conc_floor", ns))
print(grep("photic|extc|secchi|LKE_", ls(ns), value = TRUE, ignore.case = TRUE))

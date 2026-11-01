t <- readRDS(commandArgs(TRUE)[1])
print(t[t$name %in% c("aed_organic_matter/w_pom", "aed_organic_matter/rpop_hydrol", "aed_phosphorus/w_po4ads", "aed_organic_matter/rdom_minerl", "aed_alum/k_w", "aed_alum/K_P"), c("name", "value", "min", "max")])

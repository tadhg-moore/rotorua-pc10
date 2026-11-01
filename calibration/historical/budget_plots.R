# One larger figure per element from the saved budget (calibration/historical/out_budget_m46/glm_budget.rds).
suppressMessages({library(AEME); library(ggplot2)}); bud <- readRDS("calibration/historical/out_budget_m46/glm_budget.rds"); out <- "website/images/budget"
for (e in c("N", "P", "C")) ggsave(file.path(out, paste0("glm_budget_", e, ".png")), AEME:::plot_glm_budget(bud, element = e, units = "kg_yr"), width = 8, height = 7, dpi = 140, bg = "white")

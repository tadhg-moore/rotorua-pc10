# Process-level N and P budgets of the 20-year forward runs made by calibration/step2/run_forward.R, using
# nutrient_budget() from R/nutrient_budget_helpers.R.
#   Rscript calibration/step2/budget_process.R        (FWD_STAGES="b,c" by default; FWD_FROM / FWD_TO set the averaging window)
suppressMessages({library(dplyr); library(ggplot2)}); source("R/nutrient_budget_helpers.R")
stages <- strsplit(Sys.getenv("FWD_STAGES", "b,c"), ",")[[1]]
from <- Sys.getenv("FWD_FROM", "1991-01-01"); to <- Sys.getenv("FWD_TO", "2009-12-31")
odir <- file.path("website", "images", "step2c"); dir.create(odir, showWarnings = FALSE, recursive = TRUE)
res <- setNames(lapply(stages, function(s) { d <- file.path("calibration", "step2", "forward", s)
  nutrient_budget(readRDS(file.path(d, "forward_aeme.rds")), nc = file.path(d, "build", "fwd", "LID11133_rotorua", "glm_aed", "output", "output.nc"),
                  mass_balance = readRDS(file.path(d, "mass_balance_var.rds"))$daily, from = from, to = to) }), stages)
for (s in stages) { cat("\n=====", s, "=====\n"); print(res[[s]]) }
write.csv(budget_table(res, "N"), file.path(odir, "budget_N.csv"), row.names = FALSE); write.csv(budget_table(res, "P"), file.path(odir, "budget_P.csv"), row.names = FALSE)
saveRDS(res, file.path(odir, "budget_process.rds"))
ggsave(file.path(odir, "budget_process.png"), plot_nutrient_budget(res), width = 9, height = 8, dpi = 120, bg = "white")

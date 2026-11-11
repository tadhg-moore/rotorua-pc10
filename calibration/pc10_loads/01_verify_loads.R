# Check that read_pc10_loads() reproduces check_annual_loads.csv (TN, TP, FRP).
# Run from the project root:  Rscript calibration/pc10_loads/01_verify_loads.R
suppressPackageStartupMessages({library(dplyr)})
source("R/read_pc10_loads.R")
source("R/pc10_loads_helpers.R")

root <- file.path("data", "raw", "pc10_loads_2026")
chk  <- read.csv(file.path(root, "check_annual_loads.csv"))

res <- lapply(unique(chk$scenario), function(s) {
  inf  <- read_pc10_loads(s, root = root, include_rain = FALSE)
  ann  <- inflow_annual_loads(inf) |>
    group_by(hy) |>
    summarise(tn = sum(tn), tp = sum(tp), frp = sum(frp), .groups = "drop")
  ref  <- chk[chk$scenario == s, c("hy", "tn", "tp", "frp")]
  m    <- merge(ann, ref, by = "hy", suffixes = c("", "_chk"))
  data.frame(scenario = s,
             max_rel_tn  = max(abs(m$tn  / m$tn_chk  - 1)),
             max_rel_tp  = max(abs(m$tp  / m$tp_chk  - 1)),
             max_rel_frp = max(abs(m$frp / m$frp_chk - 1)))
}) |> bind_rows()

print(res, digits = 3)
cat("\nWorst relative error over all sets: ",
    signif(max(res[, -1]), 3), "\n")

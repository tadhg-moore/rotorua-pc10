suppressMessages({library(dplyr); library(tidyr)})
r <- readRDS("calibration/alum/stream/result_V1.rds"); o <- readRDS("calibration/alum/stream/result_V0.rds")
yr <- function(P, v, dmax = 5) P[[v]] |> filter(depth <= dmax, is.finite(obs), is.finite(sim)) |> group_by(yr) |> summarise(n = n(), obs = mean(obs), sim = mean(sim), .groups = "drop")
for (v in c("PHS_tp", "PHS_frp", "PHY_tchla")) { a <- yr(r$P, v); b <- yr(o$P, v); cat("\n", v, " surface 0-5 m annual mean: obs | V1 sim | V0 sim\n"); print(as.data.frame(left_join(a, b |> select(yr, sim0 = sim), by = "yr") |> mutate(across(c(obs, sim, sim0), \(x) signif(x, 3)))), row.names = FALSE) }
cat("\nalum pools (V1), lake top-5m mean by year: ala (mmol Al/m3), alp (mmol P/m3)\n"); print(as.data.frame(r$pool |> mutate(yr = as.integer(format(Date, "%Y"))) |> group_by(yr) |> summarise(ala = signif(mean(ala, na.rm = TRUE), 3), alp = signif(mean(alp, na.rm = TRUE), 3), .groups = "drop")), row.names = FALSE)

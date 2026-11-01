suppressMessages({library(AEME); library(dplyr); library(tidyr)})
inf <- AEME::inflows(readRDS("calibration/step3/aeme_base_2005_alum.rds"))$data
A <- bind_rows(lapply(c("Utuhina", "Puarenga", "Hamurana", "Waiteti"), function(n) { d <- inf[[n]]; d$Date <- as.Date(d$Date); d$yr <- as.integer(format(d$Date, "%Y"))
  d |> filter(yr >= 2000, yr <= 2022) |> group_by(yr) |> summarise(frp = mean(PHS_frp), tp = mean(PHS_frp + PHS_dop + PHS_pop + PHS_pip), pip = mean(PHS_pip), q = mean(HYD_flow) / 86400, .groups = "drop") |> mutate(stream = n) }))
print(A |> pivot_wider(id_cols = yr, names_from = stream, values_from = c(frp, tp), names_sep = "_") |> mutate(across(-yr, \(x) round(x, 4))), n = 30)
cat("\nannual TP load (kg P/yr, assuming g/m3): Utuhina vs Puarenga\n")
print(A |> filter(stream %in% c("Utuhina", "Puarenga")) |> mutate(load = tp * q * 86400 * 365 / 1000) |> select(yr, stream, load) |> pivot_wider(names_from = stream, values_from = load) |> mutate(across(-yr, round)), n = 30)

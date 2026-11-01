suppressMessages(library(dplyr))
P <- readRDS("calibration/alum/efficiency_sensitivity_pairs.rds")
per <- function(y) cut(y, c(2004, 2008, 2012, 2016, 2019, 2022), labels = c("2005-08", "2009-12", "2013-16", "2017-19", "2020-22"))
get <- function(v, dmax) bind_rows(lapply(names(P), \(s) P[[s]][[v]] |> filter(depth <= dmax) |> mutate(scale = as.numeric(s))))
tp <- get("PHS_tp", 5); frp <- get("PHS_frp", 5); chl <- get("PHY_tchla", 5)
sm <- function(x, nm) x |> mutate(per = per(yr)) |> group_by(scale, per) |> summarise(obs = mean(obs, na.rm = TRUE), sim = mean(sim, na.rm = TRUE), .groups = "drop") |> mutate(var = nm)
A <- bind_rows(sm(tp, "TP"), sm(frp, "FRP"), sm(chl, "chl"))
for (v in c("TP", "FRP", "chl")) { cat("\n", v, "(surface 0-5 m): sim mean by scale x period; obs in last column\n"); w <- A |> filter(var == v) |> mutate(sim = round(sim, 4)) |> select(scale, per, sim) |> tidyr::pivot_wider(names_from = per, values_from = sim); print(as.data.frame(w), row.names = FALSE)
  cat("obs:", paste(round((A |> filter(var == v, scale == 1))$obs, 4), collapse = " "), "\n") }
# overall bias (sim-obs) and RMSE of surface TP, FRP, chlorophyll, and bottom-DO anoxic days
met <- bind_rows(lapply(list(TP = tp, FRP = frp, chl = chl), identity), .id = "var") |> filter(is.finite(obs), is.finite(sim)) |> group_by(var, scale) |> summarise(bias_pct = 100 * (mean(sim) - mean(obs)) / mean(obs), rmse = sqrt(mean((sim - obs)^2)), r = cor(sim, obs), .groups = "drop")
cat("\nOverall surface fit:\n"); print(as.data.frame(met |> mutate(across(where(is.numeric), \(x) round(x, 3)))), row.names = FALSE)
ox <- bind_rows(lapply(names(P), \(s) P[[s]]$CHM_oxy |> filter(depth >= 17, is.finite(obs), is.finite(sim)) |> group_by(Date) |> summarise(obs = mean(obs), sim = mean(sim), .groups = "drop") |> mutate(scale = as.numeric(s)))) |>
  group_by(scale) |> summarise(obs_anoxic = sum(obs < 2), sim_anoxic = sum(sim < 2), bias = mean(sim - obs), .groups = "drop"); cat("\nBottom DO:\n"); print(as.data.frame(ox |> mutate(bias = round(bias, 3))), row.names = FALSE)

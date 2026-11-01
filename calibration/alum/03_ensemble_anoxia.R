# Bottom-anoxia fit of each alum-parameter ensemble member (alum ON, 2005-2022): observed vs simulated daily bottom DO (>= 17 m).
suppressMessages({library(dplyr); library(tidyr)})
S <- readRDS("calibration/posterior/alum_ensemble_series.rds"); B0 <- readRDS("website/images/anoxia/anoxia_o.rds")$B; obs <- B0[!is.na(B0$obs), c("Date", "obs")]
sim <- S |> filter(variant == "alum", var == "CHM_oxy", layer == "bottom") |> select(member, Date, sim = value)
D <- inner_join(sim, obs, by = "Date"); D$wy <- as.integer(format(D$Date, "%Y")) + as.integer(as.integer(format(D$Date, "%m")) >= 7)
yrs <- D |> group_by(wy) |> summarise(n = n_distinct(Date), .groups = "drop") |> filter(n >= 200) |> pull(wy)
M <- D |> group_by(member) |> summarise(bias = mean(sim - obs), rmse = sqrt(mean((sim - obs)^2)), r = cor(sim, obs), obs_anoxic = sum(obs < 2), sim_anoxic = sum(sim < 2), hit = sum(obs < 2 & sim < 2), miss = sum(obs < 2 & sim >= 2), false_alarm = sum(obs >= 2 & sim < 2), .groups = "drop")
Y <- D |> filter(wy %in% yrs) |> group_by(member, wy) |> summarise(o = sum(obs < 2), s = sum(sim < 2), .groups = "drop") |> group_by(member) |> summarise(mean_sim_days = mean(s), mean_obs_days = mean(o), r_years = cor(o, s), rmse_days = sqrt(mean((s - o)^2)), .groups = "drop")
R <- left_join(M, Y, by = "member") |> mutate(across(where(is.numeric), \(x) round(x, 2))); print(as.data.frame(R), row.names = FALSE)
R$ok <- R$sim_anoxic >= 0.75 * R$obs_anoxic & R$sim_anoxic <= 1.30 * R$obs_anoxic & R$r_years >= 0.5 & R$rmse >= 0 & R$rmse <= 1.4
cat("\nmembers passing (anoxic days within -25%/+30% of observed, year correlation >= 0.5, RMSE <= 1.4 mg/L):", paste(R$member[R$ok], collapse = ", "), "\n")
saveRDS(R, "calibration/posterior/alum_members_anoxia_check.rds"); write.csv(R, "calibration/posterior/alum_members_anoxia_check.csv", row.names = FALSE)

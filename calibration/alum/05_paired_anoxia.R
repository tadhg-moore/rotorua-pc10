# Per-member bottom-anoxia check using the observation-paired series (depth >= 17 m, daily mean), as in calibration/anoxia/01_bottom_anoxia.R.
suppressMessages(library(dplyr))
P <- readRDS("calibration/posterior/alum_ensemble_pairs.rds")
D <- bind_rows(lapply(seq_along(P), function(m) P[[m]]$CHM_oxy |> filter(depth >= 17, !is.na(obs), !is.na(sim)) |> group_by(Date) |> summarise(obs = mean(obs), sim = mean(sim), .groups = "drop") |> mutate(member = m)))
D$wy <- as.integer(format(D$Date, "%Y")) + as.integer(as.integer(format(D$Date, "%m")) >= 7)
yrs <- D |> filter(member == 1) |> group_by(wy) |> summarise(n = n(), .groups = "drop") |> filter(n >= 20) |> pull(wy)
M <- D |> group_by(member) |> summarise(bias = mean(sim - obs), rmse = sqrt(mean((sim - obs)^2)), r = cor(sim, obs), obs_anoxic = sum(obs < 2), sim_anoxic = sum(sim < 2), .groups = "drop")
Y <- D |> filter(wy %in% yrs) |> group_by(member, wy) |> summarise(o = sum(obs < 2), s = sum(sim < 2), .groups = "drop") |> group_by(member) |> summarise(r_years = cor(o, s), .groups = "drop")
R <- left_join(M, Y, by = "member") |> mutate(ratio = sim_anoxic / obs_anoxic, across(where(is.numeric), \(x) round(x, 2)))
print(as.data.frame(R), row.names = FALSE)
R$ok <- R$ratio >= 0.75 & R$ratio <= 1.30 & R$r_years >= 0.5 & R$rmse <= 1.4
cat("\npassing:", paste(R$member[R$ok], collapse = ", "), "\n")
saveRDS(R, "calibration/posterior/alum_members_anoxia_paired.rds"); write.csv(R, "calibration/posterior/alum_members_anoxia_paired.csv", row.names = FALSE)

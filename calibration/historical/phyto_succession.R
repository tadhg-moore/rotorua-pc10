# Phytoplankton succession in the best member (stage j member 46, alum on): surface (0 to 5 m) biomass of the three groups, their share through the year, and the limitation factors.
# Run: Rscript calibration/historical/phyto_succession.R   (reads out_budget_m46/member_46.rds; writes website/images/phyto, website/data/phyto)
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(lubridate); library(readr)})
d <- readRDS("calibration/historical/out_budget_m46/member_46.rds")$daily |> mutate(Date = as.Date(Date)) |> filter(Date >= "2006-07-01", Date <= "2022-06-29")
grp <- c(cyano = "Cyanobacteria", green = "Green algae", diatom = "Diatoms"); col <- c(Cyanobacteria = "#2a9d8f", `Green algae` = "#8ab17d", Diatoms = "#b5651d")
B <- d |> select(Date, all_of(paste0("PHY_", names(grp), "_surf"))) |> rename_with(~gsub("PHY_|_surf", "", .x), -Date) |> pivot_longer(-Date, names_to = "g", values_to = "b") |> mutate(group = factor(grp[g], grp))
cat("biomass unit check, summary of surface biomass:\n"); print(summary(B$b))
M <- B |> mutate(m = floor_date(Date, "month")) |> group_by(m, group) |> summarise(b = mean(b), .groups = "drop"); write_csv(M, "website/data/phyto/monthly_biomass.csv")
th <- theme_minimal(base_size = 10) + theme(legend.position = "bottom")
p1 <- ggplot(M, aes(m, b, fill = group)) + geom_area(alpha = 0.9) + scale_fill_manual(values = col) + labs(x = NULL, y = "Surface biomass (mmol C/m3), monthly mean", fill = NULL) + th
ggsave("website/images/phyto/succession_timeseries.png", p1, width = 11, height = 4, dpi = 130, bg = "white")
S <- B |> mutate(mo = month(Date), yr = year(Date)) |> group_by(mo, group) |> summarise(b = mean(b), .groups = "drop") |> group_by(mo) |> mutate(share = b / sum(b)) |> ungroup()
mn <- factor(month.abb[c(7:12, 1:6)], month.abb[c(7:12, 1:6)])
S <- S |> mutate(mon = factor(month.abb[mo], levels(mn))); write_csv(S, "website/data/phyto/seasonal_cycle.csv")
q1 <- ggplot(S, aes(mon, b, fill = group)) + geom_col() + scale_fill_manual(values = col) + labs(x = NULL, y = "Mean surface biomass (mmol C/m3)", fill = NULL) + th
q2 <- ggplot(S, aes(mon, share, fill = group)) + geom_col() + scale_fill_manual(values = col) + scale_y_continuous(labels = scales::percent) + labs(x = NULL, y = "Share of biomass", fill = NULL) + th
ggsave("website/images/phyto/succession_seasonal.png", patchwork::wrap_plots(q1, q2, guides = "collect") & theme(legend.position = "bottom"), width = 11, height = 4, dpi = 130, bg = "white")
Y <- B |> mutate(yr = year(Date - 181)) |> group_by(yr, group) |> summarise(b = mean(b), .groups = "drop") |> group_by(yr) |> mutate(share = b / sum(b)); write_csv(Y, "website/data/phyto/annual_biomass.csv")   # July to June years
p3 <- ggplot(Y, aes(factor(yr), share, fill = group)) + geom_col() + scale_fill_manual(values = col) + scale_y_continuous(labels = scales::percent) + labs(x = "Year starting 1 July", y = "Share of mean biomass", fill = NULL) + th
ggsave("website/images/phyto/succession_annual.png", p3, width = 11, height = 3.6, dpi = 130, bg = "white")
lim <- c(fI = "Light", fNit = "Nitrogen", fPho = "Phosphorus", fT = "Temperature")
L <- d |> select(Date, matches("^PHY_(cyano|green|diatom)_(fI|fNit|fPho|fT)_surf$")) |> pivot_longer(-Date, names_to = "k", values_to = "f") |> mutate(g = vapply(strsplit(k, "_"), `[`, "", 2), lf = vapply(strsplit(k, "_"), `[`, "", 3)) |>
  mutate(group = factor(grp[g], grp), limit = factor(lim[lf], lim), mon = factor(month.abb[month(Date)], levels(mn))) |> group_by(mon, group, limit) |> summarise(f = mean(f, na.rm = TRUE), .groups = "drop"); write_csv(L, "website/data/phyto/limitation_seasonal.csv")
p4 <- ggplot(L, aes(mon, f, colour = limit, group = limit)) + geom_line(linewidth = 0.8) + facet_wrap(~group) + scale_colour_manual(values = c(Light = "#e9c46a", Nitrogen = "#457b9d", Phosphorus = "#d62828", Temperature = "grey40")) + labs(x = NULL, y = "Growth factor (1 = no limitation)", colour = NULL) + th
ggsave("website/images/phyto/limitation_seasonal.png", p4, width = 11, height = 3.8, dpi = 130, bg = "white")
cat("done\n"); print(S |> group_by(group) |> slice_max(b, n = 1))

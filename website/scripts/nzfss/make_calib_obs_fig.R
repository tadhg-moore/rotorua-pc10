# Observed calibration targets through time, with the calibration periods shaded.
# Sources (targets store): lake_level, chem_data (BOPRC integrated samples, Sites 2 and 5),
# aeme_buoy_data (new buoy), calibration/all_stage/out_main/pairs_best.rds (temperature and oxygen
# observations paired to the model 2003-2022). Output: website/figures/calib_obs_periods.png

suppressMessages({library(dplyr); library(tidyr); library(ggplot2)})
st <- here::here("_targets"); rd <- function(x) targets::tar_read_raw(x, store = st)

X0 <- as.Date("1990-01-01"); X1 <- as.Date("2026-12-31")
P0 <- as.Date("2006-06-01"); CAL <- as.Date("2018-06-30"); SIM_END <- as.Date("2022-06-29")

lev <- rd("lake_level") |> transmute(Date = as.Date(Date), value = Lake_Level_m) |>
  group_by(Date) |> summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |> filter(Date >= X0)

ch <- rd("chem_data") |> filter(grepl("Integrated", LocationName)) |> mutate(Date = as.Date(Date)) |>
  group_by(Date) |>
  summarise(TP = mean(`TP (mg/m^3)`, na.rm = TRUE), FRP = mean(`DRP (mg/m^3)`, na.rm = TRUE),
            `Nitrate-N` = mean(`NNN (mg/m^3)`, na.rm = TRUE), `Chlorophyll-a` = mean(`CHLA (mg/m^3)`, na.rm = TRUE),
            .groups = "drop") |>
  pivot_longer(-Date, names_to = "series", values_to = "value") |> filter(is.finite(value), Date >= X0)

X <- readRDS(here::here("calibration", "all_stage", "out_main", "pairs_best.rds"))$P
nb <- rd("aeme_buoy_data") |> mutate(Date = as.Date(Date)) |> filter(Date > SIM_END)
tmp <- bind_rows(X$HYD_temp |> filter(depth <= 3) |> transmute(Date = as.Date(Date), value = obs),
                 nb |> filter(var_aeme == "HYD_temp", depth <= 3) |> transmute(Date, value)) |>
  group_by(Date) |> summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
oxy <- bind_rows(X$CHM_oxy |> filter(depth >= 17) |> transmute(Date = as.Date(Date), value = obs),
                 nb |> filter(var_aeme == "CHM_oxy", depth >= 17) |> transmute(Date, value)) |>
  group_by(Date) |> summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

lv <- c("Lake level (m a.s.l.)", "Surface temperature (°C)", "Bottom oxygen (g/m³)",
        "TP and FRP (mg/m³)", "Nitrate-N (mg/m³)", "Chlorophyll-a (mg/m³)")
D <- bind_rows(
  lev |> mutate(series = "Level", panel = lv[1]),
  tmp |> mutate(series = "Temperature", panel = lv[2]),
  oxy |> mutate(series = "Oxygen", panel = lv[3]),
  ch |> filter(series == "TP") |> mutate(panel = lv[4]),
  ch |> filter(series == "FRP") |> mutate(panel = lv[4]),
  ch |> filter(series == "Nitrate-N") |> mutate(panel = lv[5]),
  ch |> filter(series == "Chlorophyll-a") |> mutate(panel = lv[6])
) |> mutate(panel = factor(panel, lv),
            series = factor(series, c("Level", "Temperature", "Oxygen", "TP", "FRP", "Nitrate-N", "Chlorophyll-a")))
cat("coverage:\n"); print(D |> group_by(series) |> summarise(n = n(), from = min(Date), to = max(Date)))

per <- data.frame(
  name = factor(c("Pre-alum baseline", "Alum calibration", "Held out"),
                c("Pre-alum baseline", "Alum calibration", "Held out")),
  a = c(X0, P0, CAL), b = c(P0, CAL, SIM_END))
pcol <- c("Pre-alum baseline" = "#e8e8e8", "Alum calibration" = "#cfe3f1",
          "Held out" = "#fde8c8")
scol <- c(Level = "#1b3a4b", Temperature = "#d95f02", Oxygen = "#1b6ca8", TP = "#1b3a4b",
          FRP = "#7b3f9e", `Nitrate-N` = "#2e8b57", `Chlorophyll-a` = "#4a9a3a")

p <- ggplot() +
  geom_rect(data = per, aes(xmin = a, xmax = b, ymin = -Inf, ymax = Inf, fill = name), alpha = 0.9) +
  geom_point(data = D, aes(Date, value, colour = series), size = 0.7, alpha = 0.75) +
  facet_grid(panel ~ ., scales = "free_y", switch = "y") +
  scale_fill_manual(values = pcol, name = NULL) + scale_colour_manual(values = scol, name = NULL,
    breaks = c("TP", "FRP"), guide = guide_legend(order = 2, override.aes = list(size = 3, alpha = 1))) +
  scale_x_date(limits = c(X0, X1), breaks = as.Date(paste0(seq(1990, 2025, 5), "-01-01")),
               date_labels = "%Y", expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 18) +
  theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0, hjust = 1, size = 14, face = "bold"),
        panel.grid.minor = element_blank(), legend.position = "top", legend.box = "horizontal",
        panel.spacing.y = unit(0.4, "lines"), plot.margin = margin(4, 10, 4, 4))

out <- here::here("website", "figures", "calib_obs_periods.png")
ggsave(out, p, width = 12.8, height = 6.6, dpi = 170, bg = "white")
message("Wrote ", out)

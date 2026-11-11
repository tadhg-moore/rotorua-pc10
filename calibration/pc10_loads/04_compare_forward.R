# Compare the old and new (2026 PC10 loads) forward runs against the BoPRC lake record (sites 2 and 5) and the Mission Bay level.
#   Rscript calibration/pc10_loads/04_compare_forward.R
# Reads fwd_old/output/output.nc and fwd_new/output/output.nc (03_run_forward.ps1). Writes into calibration/pc10_loads/compare/:
#   daily_<tag>.rds (top 10 m and bottom 3 m daily means), summary.csv (bias / RMSE by period), annual.csv, and figures.
# Periods: calibration 2006-07 to 2018-06 and held-out 2018-07 to 2022-06 are the project's; 2022-07 to 2025-12 exists only for the new inflows
# (the old inflow record ends 2022-06-30) and is the out-of-sample test.
# Model P is TOT_tp plus the refractory DOP pool (0.0046 g/m3 = 0.148 mmol/m3) that TOT_tp leaves out (see memory: alum-inflows).
.libPaths(c(.libPaths(), "C:/Users/mooret/AppData/Local/R/win-library/4.5"))
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(ggplot2); library(ncdf4); library(readxl)})
out <- "calibration/pc10_loads/compare"; dir.create(out, showWarnings = FALSE, recursive = TRUE)
MWP <- 30.974; MWN <- 14.007; DOPR <- 0.148
TOPD <- 10     # integrated samples (0 to ~10 m) are compared with the model mean over the top 10 m

daily_from_nc <- function(f) {
  nc <- nc_open(f); on.exit(nc_close(nc))
  z <- ncvar_get(nc, "z"); ns <- ncvar_get(nc, "NS"); tm <- ncvar_get(nc, "time"); t0 <- ncatt_get(nc, "time", "units")$value
  orig <- as.Date(strsplit(t0, " ")[[1]][3]); dd <- if (grepl("hour", t0)) orig + floor(tm / 24) else orig + tm
  nl <- nrow(z); nt <- length(ns); valid <- row(z) <= matrix(ns, nl, nt, byrow = TRUE)
  zt <- ifelse(valid, z, NA); top <- apply(zt, 2, max, na.rm = TRUE)                 # surface height
  zb <- rbind(0, zt[-nl, , drop = FALSE]); dz <- ifelse(valid, z - zb, 0)            # layer thickness (z = layer top height above bed)
  depth_mid <- top - (zt + zb) / 2
  wtop <- dz * (valid & depth_mid <= TOPD); wbot <- dz * (valid & zt <= matrix(apply(zt, 2, min, na.rm = TRUE) + 3, nl, nt, byrow = TRUE))
  wm <- function(a, w) colSums(ifelse(is.na(a), 0, a) * w) / pmax(colSums(w), 1e-9)
  vars <- intersect(c("TOT_tp", "TOT_tn", "PHS_frp", "NIT_nit", "NIT_amm", "PHY_tchla", "OXY_oxy", "temp", "ALU_ala", "ALU_alp"), names(nc$var))
  res <- data.frame(Date = dd)
  for (v in vars) { a <- ncvar_get(nc, v); a[!is.finite(a) | abs(a) > 1e20] <- NA
    res[[paste0(v, "_top")]] <- wm(a, wtop); res[[paste0(v, "_bot")]] <- wm(a, wbot) }
  res$level <- if ("lake_level" %in% names(nc$var)) ncvar_get(nc, "lake_level") else top
  res$depth <- top
  res
}
dl <- list()
for (tg in c("old", "new", "newsil")) {
  if (!file.exists(file.path("calibration/pc10_loads", paste0("fwd_", tg), "output", "output.nc"))) next
  f <- file.path("calibration/pc10_loads", paste0("fwd_", tg), "output", "output.nc"); of <- file.path(out, paste0("daily_", tg, ".rds"))
  if (!file.exists(of) || file.mtime(of) < file.mtime(f)) saveRDS(daily_from_nc(f), of)
  dl[[tg]] <- readRDS(of)
}
cat("model ranges: old", format(range(dl$old$Date)), "| new", format(range(dl$new$Date)), "\n")

# ---- observations --------------------------------------------------------------------------------------------------
fo <- "data/processed/pc10_data/Rotorua S2 & S5 data 1989_2026.xlsx"
w <- bind_rows(read_excel(fo, sheet = "Rotorua S5 data 1989_2026"), read_excel(fo, sheet = "Rotorua S2 data 1989_2026")) |>
  filter(grepl("Integrated", LocationName)) |> mutate(Date = as.Date(Date))
obs_w <- w |> group_by(Date) |>                                                      # mean of the two sites on a date
  summarise(TP = mean(`TP (mg/m^3)`, na.rm = TRUE), TN = mean(`TN (mg/m^3)`, na.rm = TRUE), FRP = mean(`DRP (mg/m^3)`, na.rm = TRUE),
            chl = mean(`CHLA (mg/m^3)`, na.rm = TRUE), NO3 = mean(`NNN (mg/m^3)`, na.rm = TRUE), NH4 = mean(`NH4-N (mg/m^3)`, na.rm = TRUE), .groups = "drop") |>
  mutate(across(-Date, \(x) ifelse(is.nan(x), NA, x))) |> pivot_longer(-Date, names_to = "var", values_to = "obs") |> filter(!is.na(obs))
fp <- "data/processed/pc10_data/Lake Rotorua S2 & S5 Profile data 2001_2026.xlsx"
p <- read_excel(fp) |> mutate(Date = as.Date(Date)) |> rename(depth = `Depth (m)`, DO = `Dissolved oxygen (g/m^3)`) |> filter(!is.na(DO))
obs_do <- p |> group_by(Site, Date) |> filter(max(depth) >= 17) |> summarise(obs = mean(DO[depth >= max(depth) - 3]), .groups = "drop") |>
  group_by(Date) |> summarise(obs = mean(obs), .groups = "drop") |> mutate(var = "DO_bot")

mod_long <- function(d, tg) {
  mt <- d |> transmute(Date, TP = (TOT_tp_top + DOPR) * MWP, TN = TOT_tn_top * MWN, FRP = PHS_frp_top * MWP, chl = PHY_tchla_top,
                       NO3 = NIT_nit_top * MWN, NH4 = NIT_amm_top * MWN, DO_bot = OXY_oxy_bot * 32 / 1000)
  pivot_longer(mt, -Date, names_to = "var", values_to = "sim") |> mutate(run = tg)
}
pairs <- bind_rows(lapply(names(dl), \(tg) inner_join(bind_rows(obs_w, obs_do), mod_long(dl[[tg]], tg), by = c("Date", "var")))) |>
  mutate(period = case_when(Date < "2006-07-01" ~ "1 pre-2006", Date <= "2018-06-30" ~ "2 calibration 2006-18", Date <= "2022-06-30" ~ "3 held out 2018-22",
                            TRUE ~ "4 new years 2022-25"))
summ <- pairs |> group_by(var, period, run) |>
  summarise(n = n(), obs_mean = mean(obs), sim_mean = mean(sim), bias_pct = 100 * (mean(sim) / mean(obs) - 1), rmse = sqrt(mean((sim - obs)^2)),
            r = suppressWarnings(cor(sim, obs)), .groups = "drop")
write.csv(summ, file.path(out, "summary.csv"), row.names = FALSE)
print(summ |> mutate(across(where(is.numeric), \(x) round(x, 2))) |> arrange(var, period, run), n = 100)

# ---- level ---------------------------------------------------------------------------------------------------------
source("R/extract_pc10_data.R")
lv <- read_aquarius_csv("data/processed/pc10_data/Lake Rotorua at Mission Bay Day Mean Lake Level.EntireRecord.csv") |> mutate(Date = as.Date(datetime)) |>
  group_by(Date) |> summarise(obs = mean(value, na.rm = TRUE), .groups = "drop")
lvl <- bind_rows(lapply(names(dl), \(tg) transmute(dl[[tg]], Date, sim = level, run = tg))) |> inner_join(lv, by = "Date")
ref <- lvl |> filter(Date >= "2002-01-01", Date <= "2012-12-31") |> group_by(run) |> summarise(so = mean(sim), oo = mean(obs))
lvl <- lvl |> left_join(ref, by = "run") |> mutate(sim_a = sim - so, obs_a = obs - oo)
lvl_s <- lvl |> mutate(yr = as.integer(format(Date, "%Y"))) |> group_by(run, yr) |> summarise(bias_cm = 100 * mean(sim_a - obs_a), .groups = "drop")
write.csv(lvl_s, file.path(out, "level_bias_by_year.csv"), row.names = FALSE)
print(lvl_s |> pivot_wider(names_from = run, values_from = bias_cm) |> mutate(across(-yr, \(x) round(x, 1))), n = 40)

# ---- annual means and figures ---------------------------------------------------------------------------------------
ann <- bind_rows(lapply(names(dl), \(tg) mod_long(dl[[tg]], tg))) |> mutate(yr = as.integer(format(Date, "%Y"))) |> filter(yr >= 2003) |>
  group_by(run, var, yr) |> summarise(sim = mean(sim, na.rm = TRUE), .groups = "drop")
oa <- obs_w |> bind_rows(obs_do) |> mutate(yr = as.integer(format(Date, "%Y"))) |> group_by(var, yr) |> summarise(obs = mean(obs), .groups = "drop")
ann <- ann |> left_join(oa, by = c("var", "yr")); write.csv(ann, file.path(out, "annual.csv"), row.names = FALSE)
g <- ggplot(ann |> filter(var %in% c("TP", "FRP", "chl", "TN")), aes(yr)) + geom_line(aes(y = sim, colour = run)) + geom_point(aes(y = obs), colour = "black", size = 1) +
  geom_vline(xintercept = 2022.5, linetype = 2, colour = "grey50") + facet_wrap(~var, scales = "free_y", ncol = 1) +
  scale_colour_manual(values = c(old = "grey40", new = "#0072B2", newsil = "#D55E00")) + labs(x = NULL, y = "Annual mean, top 10 m (mg/m3)", colour = NULL) + theme_bw()
ggsave(file.path(out, "annual_compare.png"), g, width = 9, height = 9, dpi = 150)
g2 <- ggplot(lvl_s, aes(yr, bias_cm, colour = run)) + geom_hline(yintercept = 0, colour = "grey70") + geom_line() +
  scale_colour_manual(values = c(old = "grey40", new = "#0072B2", newsil = "#D55E00")) + labs(x = NULL, y = "Model - observed level anomaly (cm)", colour = NULL) + theme_bw()
ggsave(file.path(out, "level_bias.png"), g2, width = 9, height = 4, dpi = 150)
cat("done\n")

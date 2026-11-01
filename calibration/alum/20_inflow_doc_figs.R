# Figures and small summary tables for website/rotorua-inflows-alum.qmd (the inflow files themselves are ~100 MB and are not committed).
suppressMessages({library(dplyr); library(tidyr); library(readr); library(ggplot2); library(AEME)})
out <- "website/images/inflows"; dat <- "website/data/inflows"; dir.create(out, recursive = TRUE, showWarnings = FALSE); dir.create(dat, recursive = TRUE, showWarnings = FALSE)
rd <- function(f) { x <- read_csv(file.path("data/raw/flows", f), show_col_types = FALSE); x$Date <- as.Date(x$Date); x$yr <- as.integer(format(x$Date, "%Y")); x$mo <- as.integer(format(x$Date, "%m")); x }
w <- rd("rotorua_inflows_populated_withalum.csv"); n <- rd("rotorua_inflows_populated_noalum.csv")
th <- theme_minimal(base_size = 10) + theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
major <- c("Awahou", "Hamurana", "Ngongotaha", "Puarenga", "Utuhina", "Waingaehe", "Waiohewa", "Waiowhiro", "Waiteti")
dosed <- c("Utuhina", "Puarenga")
# dose (kg Al/d) from the model inflow files (capacity in mmol/m3, ratio 1; flow in m3/d)
inf <- AEME::inflows(readRDS("calibration/step3/aeme_base_2005_alum.rds"))$data
dose <- bind_rows(lapply(dosed, function(s) { x <- inf[[s]]; tibble(Date = as.Date(x$Date), Inflow = s, ala = x$ALU_ala, Q = x$HYD_flow) })) |> mutate(al_kgd = ala * Q * 26.98 / 1e6)
sv <- function(name, p, w, h) ggsave(file.path(out, name), p, width = w, height = h, dpi = 130, bg = "white")

# F1: annual mean DRP and TP of the nine major streams (no-alum file = untreated)
a1 <- n |> filter(Inflow %in% major) |> group_by(Inflow, yr) |> summarise(DRP = mean(DRP), TP = mean(TP), .groups = "drop") |> pivot_longer(c(DRP, TP), names_to = "var", values_to = "g_m3")
sv("annual_stream_P.png", ggplot(a1, aes(yr, g_m3, colour = var)) + geom_line(linewidth = 0.6) + facet_wrap(~Inflow, scales = "free_y", ncol = 3) +
     labs(x = NULL, y = "Annual mean concentration (g P/m3)", colour = NULL, subtitle = "Untreated inflow P (no-alum file)") + th, 9, 6)

# F2: dosed streams, annual DRP with vs without alum, and the Al dose
dd <- bind_rows(w |> filter(Inflow %in% dosed) |> mutate(file = "with-alum file"), n |> filter(Inflow %in% dosed) |> mutate(file = "no-alum file"))
a2 <- dd |> filter(yr >= 2000, yr <= 2022) |> group_by(Inflow, file, yr) |> summarise(DRP = mean(DRP), .groups = "drop")
al <- dose |> mutate(yr = as.integer(format(Date, "%Y"))) |> filter(yr >= 2000) |> group_by(Inflow, yr) |> summarise(al_t = sum(al_kgd, na.rm = TRUE) / 1000, .groups = "drop")
p2a <- ggplot(a2, aes(yr, DRP, colour = file, linetype = file)) + geom_line(linewidth = 0.7) + geom_point(size = 1) + facet_wrap(~Inflow, ncol = 2) + labs(x = NULL, y = "Annual mean DRP (g P/m3)", colour = NULL, linetype = NULL) + th
p2b <- ggplot(al, aes(yr, al_t)) + geom_col(fill = "grey60") + facet_wrap(~Inflow, ncol = 2) + labs(x = NULL, y = "Al dosed (t/yr)") + th
sv("dosed_streams_annual.png", gridExtra::grid.arrange(p2a, p2b, heights = c(2, 1)), 9, 6.5)

# F3: daily, July 2011 to July 2013 (Hamilton 2015 Fig 19 analogue)
d3 <- dd |> filter(Date >= "2011-07-01", Date <= "2013-07-31") |> select(Date, Inflow, file, DRP) |> left_join(dose |> select(Date, Inflow, al_kgd), by = c("Date", "Inflow"))
sv("daily_2011_2013.png", ggplot(d3, aes(Date)) + geom_col(data = distinct(d3, Date, Inflow, al_kgd), aes(y = al_kgd / 4000), fill = "grey80", width = 1) + geom_line(aes(y = DRP, colour = file), linewidth = 0.4) +
     facet_wrap(~Inflow, ncol = 1) + scale_y_continuous(name = "DRP (g P/m3)", sec.axis = sec_axis(~ . * 4000, name = "Al dose (kg/d, grey)")) + labs(x = NULL, colour = NULL) + th, 9, 6)

# F4: stream DRP removal vs dose and vs Al:DRP
f4 <- inner_join(w |> filter(Inflow %in% dosed) |> select(Date, Inflow, DRP_w = DRP), n |> filter(Inflow %in% dosed) |> select(Date, Inflow, DRP_n = DRP), by = c("Date", "Inflow")) |>
  inner_join(dose, by = c("Date", "Inflow")) |> filter(Date >= "2007-01-01", ala > 0, DRP_n > 0.005) |> mutate(R = ala / (DRP_n * 1000 / 30.974), f = pmin(pmax(1 - DRP_w / DRP_n, 0), 1))
fit <- lapply(split(f4, f4$Inflow), function(z) coef(nls(f ~ fmax * (1 - exp(-R / R0)), z, start = list(fmax = 0.8, R0 = 10), algorithm = "port", lower = c(0.05, 0.5), upper = c(1, 200))))
fits <- bind_rows(lapply(names(fit), function(s) tibble(Inflow = s, fmax = fit[[s]][["fmax"]], R0 = fit[[s]][["R0"]], n_days = sum(f4$Inflow == s), mean_f = mean(f4$f[f4$Inflow == s]))))
write_csv(fits |> mutate(across(where(is.numeric), \(x) round(x, 3))), file.path(dat, "stream_removal_fit.csv"))
cv <- bind_rows(lapply(names(fit), function(s) tibble(Inflow = s, R = seq(0.5, 120, 0.5)) |> mutate(f = fit[[s]][["fmax"]] * (1 - exp(-R / fit[[s]][["R0"]])))))
pa <- ggplot(f4, aes(al_kgd, f)) + geom_point(alpha = 0.15, size = 0.6) + geom_smooth(se = FALSE, colour = "red", method = "gam") + geom_vline(xintercept = 75, linetype = 2) + facet_wrap(~Inflow) + coord_cartesian(xlim = c(0, 400)) + labs(x = "Al dose (kg/d)", y = "DRP removal fraction") + th
pb <- ggplot(f4, aes(R, f)) + geom_point(alpha = 0.15, size = 0.6) + geom_line(data = cv, colour = "red", linewidth = 0.9) + geom_vline(xintercept = 20, linetype = 2) + facet_wrap(~Inflow) + coord_cartesian(xlim = c(0, 120)) + labs(x = "Al : DRP (mol/mol, dosed capacity / untreated DRP)", y = "DRP removal fraction") + th
sv("removal_vs_dose.png", gridExtra::grid.arrange(pa, pb), 9, 7)

# F5: Utuhina DRP:TP by month, pre and post alum (Hamilton 2015 Figs 17-18 analogue)
u <- bind_rows(w |> filter(Inflow == "Utuhina") |> mutate(file = "measured (with-alum file)"), n |> filter(Inflow == "Utuhina") |> mutate(file = "reconstructed untreated (no-alum file)")) |> filter(yr >= 2001, yr <= 2012) |> mutate(r = DRP / TP, period = ifelse(Date < "2006-06-01", "pre-alum", "post-alum"))
m5 <- u |> group_by(file, period, mo) |> summarise(r = median(r), .groups = "drop")
sv("utuhina_drp_tp.png", ggplot(m5, aes(mo, r, colour = paste(period, "|", file))) + geom_line(linewidth = 0.8) + geom_point() + scale_x_continuous(breaks = 1:12) + labs(x = "Month", y = "Median DRP / TP", colour = NULL, subtitle = "Utuhina inflow") + th + guides(colour = guide_legend(nrow = 2)), 8, 4.2)
write_csv(m5 |> pivot_wider(names_from = c(file, period), values_from = r) |> mutate(across(where(is.numeric), \(x) round(x, 3))), file.path(dat, "utuhina_drp_tp_monthly.csv"))

# F6: P forms delivered by the dosed streams, with vs without alum
ld <- bind_rows(w |> mutate(file = "with-alum"), n |> mutate(file = "no-alum")) |> filter(Inflow %in% dosed, yr >= 2005, yr <= 2022) |>
  group_by(file, Inflow, yr) |> summarise(across(c(DRP, DOPL, POPL, PIP), \(x) sum(x * VOL, na.rm = TRUE) / 1000), .groups = "drop") |> pivot_longer(c(DRP, DOPL, POPL, PIP), names_to = "form", values_to = "kg")
sv("dosed_P_forms.png", ggplot(ld, aes(yr, kg, fill = form)) + geom_col() + facet_grid(Inflow ~ file, scales = "free_y") + labs(x = NULL, y = "P load to the lake (kg P/yr)", fill = NULL) + th, 9, 6)

# F7: total P load to the lake by source group (no-alum file)
grp <- c(Awahou = "Groundwater-fed streams", Hamurana = "Groundwater-fed streams", Puarenga = "Puarenga + Utuhina", Utuhina = "Puarenga + Utuhina", Ngongotaha = "Other streams", Waingaehe = "Other streams", Waiohewa = "Other streams", Waiowhiro = "Other streams", Waiteti = "Other streams",
         Geothermal = "Geothermal", `Ungauged-Slow` = "Ungauged", `Ungauged-Quick` = "Ungauged", Rainfall = "Rainfall", WWTP = "WWTP")
tl <- n |> filter(yr >= 1965, yr <= 2021) |> mutate(group = grp[Inflow], tp_load = TP * VOL / 1e6) |> group_by(group, yr) |> summarise(t = sum(tp_load, na.rm = TRUE), .groups = "drop")
sv("total_P_load.png", ggplot(tl, aes(yr, t, fill = group)) + geom_col() + geom_hline(yintercept = 37, linetype = 2, colour = "red") + annotate("text", x = 1995, y = 33, label = "37 t TP/yr sustainable load", hjust = 0, size = 3, colour = "red") +
     labs(x = NULL, y = "TP load (t P/yr)", fill = NULL, subtitle = "No-alum inflow file, all sources (includes stormflow particulates)") + th, 9, 4.8)
write_csv(tl |> mutate(period = cut(yr, c(1964, 1990, 2000, 2010, 2022), labels = c("1965-90", "1991-2000", "2001-10", "2011-22"))) |> group_by(period, group) |> summarise(t_per_yr = round(sum(t) / n_distinct(yr), 1), .groups = "drop") |> pivot_wider(names_from = group, values_from = t_per_yr), file.path(dat, "tp_load_by_period.csv"))

# tables: what the two files change
j <- inner_join(w, n, by = c("Date", "InfNum"), suffix = c("_w", "_n")) |> filter(yr_w >= 2000, yr_w <= 2022)
tab <- j |> group_by(Inflow = Inflow_w) |> summarise(DRP = 100 * (sum(DRP_w) - sum(DRP_n)) / sum(DRP_n), PIP = 100 * (sum(PIP_w) - sum(PIP_n)) / sum(PIP_n), TP = 100 * (sum(TP_w) - sum(TP_n)) / sum(TP_n), .groups = "drop") |> filter(abs(DRP) > 0.05 | abs(PIP) > 0.05 | abs(TP) > 0.05)
write_csv(tab |> mutate(across(where(is.numeric), \(x) round(x, 1))), file.path(dat, "files_difference.csv"))
cat("done\n"); print(as.data.frame(fits)); print(as.data.frame(tab))

# Figures for the sensitivity analysis write-up. Writes PNGs and small CSVs to
# website/images/sensitivity/. Run after the SA runs have been analysed (sa_analyse.R).
#   Rscript calibration/sa/sa_plots.R
suppressMessages({ library(dplyr); library(tidyr); library(ggplot2) })
out_dir <- "website/images/sensitivity"; dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
tg <- read.csv("calibration/csps_targets.csv")

variants <- c(alum = "calibration/sa_runs", noalum = "calibration/sa_runs_noalum",
              hourly = "calibration/sa_runs_hourly", dailyw2 = "calibration/sa_runs_dailyw2")
read_eff <- function(v) {
  f <- file.path(variants[[v]], "sa_effects.csv")
  if (!file.exists(f)) return(NULL)
  e <- read.csv(f); e$variant <- v; e
}
E <- Filter(Negate(is.null), lapply(names(variants), read_eff))
names(E) <- vapply(E, \(x) x$variant[1], "")
for (v in names(E)) file.copy(file.path(variants[[v]], "sa_effects.csv"),
                              file.path(out_dir, paste0("sa_effects_", v, ".csv")), overwrite = TRUE)

for (v in names(E)) file.copy(file.path(variants[[v]], "sa_runs_table.csv"),
                              file.path(out_dir, paste0("sa_runs_", v, ".csv")), overwrite = TRUE)

file.copy("calibration/sa_runs/sa_table.rds", file.path(out_dir, "sa_table.rds"), overwrite = TRUE)
resp_cat <- c(mix = "Polymixis", anox = "Anoxia", load = "Internal loading", chl = "Phytoplankton",
              sys = "System", st = "State bias")
resp_label <- function(o) sub("^(mix|anox|load|chl|sys)_", "", o)
state_keep <- c("st_HYD_temp_surface_bias", "st_HYD_temp_bottom_bias", "st_CHM_oxy_surface_bias",
                "st_CHM_oxy_bottom_bias", "st_PHY_tchla_surface_bias", "st_PHS_frp_bottom_bias",
                "st_NIT_amm_bottom_bias", "st_PHS_tp_surface_bias")
theme_order <- c("physics", "sediment", "organic matter", "nitrogen", "phosphorus", "phytoplankton", "alum")

prep <- function(e, cap = 10) {
  e <- e[e$output %in% c(tg$signature, state_keep), ]
  e$cat <- ifelse(grepl("^st_", e$output), "st", sub("_.*", "", e$output))
  e$resp <- ifelse(e$cat == "st", sub("^st_", "", sub("_bias$", "", e$output)), resp_label(e$output))
  e$val <- pmin(e$mu_star_scaled, cap)
  e$theme <- factor(e$theme, theme_order)
  e
}

# Heatmap: every parameter (rows, grouped by process area and ordered by influence) against every
# CSPS response (columns, grouped by target). Fill = elementary-effect size across the full
# parameter range in tolerance units (1 = moves the metric by one year-to-year sd).
heatmap_plot <- function(e, title, file, cap = 10) {
  d <- prep(e, cap)
  ord <- d |> group_by(parameter, theme) |> summarise(t = sum(val, na.rm = TRUE), .groups = "drop") |>
    arrange(theme, desc(t))
  d$parameter <- factor(d$parameter, rev(unique(ord$parameter)))
  d$cat <- factor(resp_cat[d$cat], resp_cat)
  rord <- unique(d[order(d$cat, match(d$output, c(tg$signature, state_keep))), c("cat", "resp")])
  d$resp <- factor(d$resp, unique(rord$resp))
  d$sign <- ifelse(d$val >= 1, ifelse(d$mu > 0, "+", "−"), "")
  # row facets by theme
  p <- ggplot(d, aes(resp, parameter, fill = val)) +
    geom_tile(colour = "white", linewidth = 0.2) +
    geom_text(aes(label = sign), size = 2.2, colour = "white") +
    facet_grid(theme ~ cat, scales = "free", space = "free", switch = "y") +
    scale_fill_viridis_c(name = "Effect\n(tolerances)", option = "magma", direction = -1,
                         limits = c(0.3, cap), oob = scales::squish, trans = "log10",
                         breaks = c(0.3, 1, 3, 10), labels = c("<0.3", "1", "3", "10+")) +
    labs(title = title, x = NULL, y = NULL,
         subtitle = "Morris mean absolute elementary effect over the full parameter range, in units of the interannual sd of the observed metric. + / −: direction of change when the parameter increases (effects of 1 or more).") +
    theme_minimal(base_size = 9) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7), axis.text.y = element_text(size = 6.5),
          strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold"),
          strip.text.x = element_text(face = "bold"), strip.placement = "outside",
          panel.grid = element_blank(), panel.spacing = unit(0.15, "lines"),
          legend.position = "right")
  ggsave(file.path(out_dir, file), p, width = 15, height = 21, dpi = 110, bg = "white")
  invisible(d)
}

# Overall influence by parameter (bar chart of the strongest 30)
influence_plot <- function(e, title, file, n = 30) {
  d <- prep(e)
  t <- d |> group_by(parameter, theme) |> summarise(m = mean(val, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(m)) |> head(n)
  t$parameter <- factor(t$parameter, rev(t$parameter))
  p <- ggplot(t, aes(parameter, m, fill = theme)) + geom_col() + coord_flip() +
    scale_fill_brewer(palette = "Set2", name = NULL) +
    labs(title = title, x = NULL, y = "Mean effect across all responses (tolerances, capped at 10)") +
    theme_minimal(base_size = 10)
  ggsave(file.path(out_dir, file), p, width = 8, height = 7, dpi = 120, bg = "white")
}

# Rank agreement between two variants
compare_plot <- function(a, b, la, lb, file) {
  f <- function(e) prep(e) |> group_by(parameter, theme) |> summarise(m = mean(val, na.rm = TRUE), .groups = "drop")
  m <- inner_join(f(a), f(b), by = c("parameter", "theme"), suffix = c("_a", "_b"))
  rho <- cor(m$m_a, m$m_b, method = "spearman")
  lab <- m[m$m_a > quantile(m$m_a, 0.9) | m$m_b > quantile(m$m_b, 0.9) | abs(m$m_a - m$m_b) > 1.5, ]
  p <- ggplot(m, aes(m_a, m_b, colour = theme)) + geom_abline(slope = 1, intercept = 0, colour = "grey60") +
    geom_point(size = 2) + geom_text(data = lab, aes(label = parameter), size = 2.6, show.legend = FALSE, check_overlap = TRUE, vjust = -0.7) +
    scale_colour_brewer(palette = "Set2", name = NULL) +
    labs(x = paste("Mean effect:", la), y = paste("Mean effect:", lb),
         title = sprintf("%s vs %s (Spearman rho = %.2f)", la, lb, rho)) +
    theme_minimal(base_size = 10)
  ggsave(file.path(out_dir, file), p, width = 8, height = 7, dpi = 120, bg = "white")
  rho
}

if ("alum" %in% names(E)) { heatmap_plot(E$alum, "Parameter to response map: daily forcing, alum on", "heatmap_alum.png")
  influence_plot(E$alum, "Strongest parameters, daily forcing with alum", "influence_alum.png") }
if ("noalum" %in% names(E)) heatmap_plot(E$noalum, "Parameter to response map: daily forcing, no alum", "heatmap_noalum.png")
if ("hourly" %in% names(E)) { heatmap_plot(E$hourly, "Parameter to response map: hourly forcing, alum on", "heatmap_hourly.png")
  influence_plot(E$hourly, "Strongest parameters, hourly forcing with alum", "influence_hourly.png") }
if ("dailyw2" %in% names(E)) heatmap_plot(E$dailyw2, "Parameter to response map: daily forcing (same window as hourly)", "heatmap_dailyw2.png")
rho <- list()
if (all(c("alum", "noalum") %in% names(E))) rho$alum <- compare_plot(E$alum, E$noalum, "alum on", "alum off", "compare_alum.png")
if (all(c("hourly", "dailyw2") %in% names(E))) rho$forcing <- compare_plot(E$hourly, E$dailyw2, "hourly forcing", "daily forcing (same window)", "compare_forcing.png")
if (all(c("alum", "dailyw2") %in% names(E))) rho$window <- compare_plot(E$alum, E$dailyw2, "daily, 2010-13 window", "daily, 2012-15 window", "compare_window.png")
saveRDS(rho, file.path(out_dir, "rank_correlations.rds"))
print(rho)

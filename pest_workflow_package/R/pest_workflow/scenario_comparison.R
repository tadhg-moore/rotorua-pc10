# How much does each inflow scenario change the lake, relative to the baseline?
#
# Uses the PAIRED ensembles: member run_<id> in a scenario shares its exact
# parameter set with member run_<id> in the baseline, so each change is
# computed per member (scenario - baseline) and the spread across members is the
# parameter uncertainty on that CHANGE - not just two overlapping distributions.
#
# Input : <LOCAL_ROOT>/<lake>/summaries/<scenario>_member_daily.parquet
#         (built by member_daily.R; already trimmed to simulation_time)
# Output: pest_workflow/<lake>/comparison/
#   scenario_vs_baseline.csv   per scenario x variable x depth class x period:
#       baseline mean, median change, 5-95% interval of change, median %
#       change (+ interval), P(decrease), and robust = interval excludes 0
#   annual_means.csv           annual means (median, 5-95%) per scenario
#   pct_change.png             forest plot of % change (annual-mean metric)
#   annual_<var>.png           annual mean time series, baseline vs scenarios
#   extremes.csv               chl-a 95th percentile, days of bottom hypoxia
#
# Usage: Rscript R/pest_workflow/scenario_comparison.R <LAKE_TAG>

suppressMessages({ library(dplyr); library(tidyr); library(arrow); library(ggplot2) })
source("R/pest_workflow/manifest.R")

MD_ROOT <- Sys.getenv("PEST_LOCAL_ROOT", "C:/Users/data/Desktop/ac_lake_calib/pest_workflow")   # EDIT default or set PEST_LOCAL_ROOT
KEY_VARS <- c("PHY_tchla", "NIT_tn", "PHS_tp", "NCS_ss1", "CHM_oxy", "HYD_temp",
              "NIT_amm", "PHS_frp", "LKE_lvlwtr")
SEASONS <- c(all = "all", summer = "Dec-Feb", winter = "Jun-Aug")   # southern hemisphere
season_of <- function(d) {
  m <- as.integer(format(d, "%m"))
  ifelse(m %in% c(12, 1, 2), "summer", ifelse(m %in% 6:8, "winter", "other"))
}

compare_scenarios <- function(lake_tag) {
  out_dir <- file.path("pest_workflow", lake_tag, "comparison")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  sdir <- file.path(MD_ROOT, lake_tag, "summaries")
  files <- list.files(sdir, pattern = "_member_daily\\.parquet$", full.names = TRUE)
  stopifnot(any(grepl("/baseline_member_daily", files)))
  d <- bind_rows(lapply(files, read_parquet))
  scens <- setdiff(sort(unique(d$scenario)), "baseline")
  message(sprintf("[%s] baseline + %d scenario(s): %s", lake_tag, length(scens), paste(scens, collapse = ", ")))

  # members present in baseline AND every scenario, minus baseline numerically-unstable runs
  mem_by <- split(d$member, d$scenario); mem_by <- lapply(mem_by, unique)
  common <- Reduce(intersect, mem_by)
  b <- d |> filter(scenario == "baseline")
  bad <- union(
    b |> filter(variable == "HYD_temp", depth_class == "bottom") |> summarise(lo = min(sim, na.rm = TRUE),
          hi = max(sim, na.rm = TRUE), .by = member) |> filter(lo < 2 | hi > 32) |> pull(member),
    b |> filter(variable == "PHY_tchla") |> summarise(mx = max(sim, na.rm = TRUE), .by = member) |>
      filter(mx > 500) |> pull(member))
  common <- setdiff(common, bad)
  message(sprintf("  %d paired members (dropped %d unstable, %d not present in all runs)", length(common),
                  length(bad), length(setdiff(unique(d$member), union(common, bad)))))
  d <- d |> filter(member %in% common) |> mutate(season = season_of(Date), year = as.integer(format(Date, "%Y")))

  # ---- per-member period means, then paired change --------------------------------
  pm <- bind_rows(
    d |> summarise(m = mean(sim, na.rm = TRUE), .by = c(scenario, member, variable, depth_class)) |> mutate(period = "all"),
    d |> filter(season %in% c("summer", "winter")) |>
      summarise(m = mean(sim, na.rm = TRUE), .by = c(scenario, member, variable, depth_class, season)) |>
      rename(period = season))
  paired <- pm |> filter(scenario != "baseline") |>
    inner_join(pm |> filter(scenario == "baseline") |> select(member, variable, depth_class, period, base = m),
               by = c("member", "variable", "depth_class", "period")) |>
    mutate(delta = m - base, pct = ifelse(abs(base) > 1e-9, 100 * delta / base, NA_real_))
  summ <- paired |>
    summarise(baseline_mean = median(base), delta_med = median(delta),
              delta_q05 = quantile(delta, .05), delta_q95 = quantile(delta, .95),
              pct_med = median(pct, na.rm = TRUE), pct_q05 = quantile(pct, .05, na.rm = TRUE),
              pct_q95 = quantile(pct, .95, na.rm = TRUE),
              p_decrease = mean(delta < 0), n_members = n(),
              .by = c(scenario, variable, depth_class, period)) |>
    mutate(robust = delta_q05 > 0 | delta_q95 < 0) |>
    arrange(variable, depth_class, period, scenario)
  write.csv(summ, file.path(out_dir, "scenario_vs_baseline.csv"), row.names = FALSE)

  # ---- extremes -------------------------------------------------------------------
  ext <- bind_rows(
    d |> filter(variable == "PHY_tchla", depth_class == "surface") |>
      summarise(value = quantile(sim, .95, na.rm = TRUE), .by = c(scenario, member)) |> mutate(metric = "chla_surface_p95"),
    d |> filter(variable == "CHM_oxy", depth_class == "bottom") |>
      summarise(value = 100 * mean(sim < 2, na.rm = TRUE), .by = c(scenario, member)) |> mutate(metric = "pct_days_bottom_oxy_lt2"))
  ext_p <- ext |> filter(scenario != "baseline") |>
    inner_join(ext |> filter(scenario == "baseline") |> select(member, metric, base = value), by = c("member", "metric")) |>
    mutate(delta = value - base) |>
    summarise(baseline = median(base), change_med = median(delta), change_q05 = quantile(delta, .05),
              change_q95 = quantile(delta, .95), .by = c(scenario, metric))
  write.csv(ext_p, file.path(out_dir, "extremes.csv"), row.names = FALSE)

  # ---- annual means ---------------------------------------------------------------
  ann <- d |> summarise(m = mean(sim, na.rm = TRUE), .by = c(scenario, member, variable, depth_class, year)) |>
    summarise(med = median(m), q05 = quantile(m, .05), q95 = quantile(m, .95), .by = c(scenario, variable, depth_class, year))
  write.csv(ann, file.path(out_dir, "annual_means.csv"), row.names = FALSE)

  # ---- figures --------------------------------------------------------------------
  fp <- summ |> filter(period == "all", variable %in% KEY_VARS, depth_class != "mid" | variable %in% c("HYD_temp", "CHM_oxy")) |>
    mutate(panel = paste(variable, depth_class, sep = " | "))
  p1 <- ggplot(fp, aes(pct_med, scenario, colour = robust)) +
    geom_vline(xintercept = 0, colour = "grey50") +
    geom_pointrange(aes(xmin = pct_q05, xmax = pct_q95), size = .3) +
    scale_colour_manual(values = c(`TRUE` = "firebrick", `FALSE` = "grey40"),
                        labels = c(`TRUE` = "90% interval excludes 0", `FALSE` = "includes 0"), name = NULL) +
    facet_wrap(~ panel, scales = "free_x") +
    labs(title = sprintf("%s | change vs baseline, period mean (median and 5-95%% across paired members)", lake_tag),
         x = "% change", y = NULL) + theme_bw(base_size = 8) + theme(legend.position = "bottom")
  ggsave(file.path(out_dir, "pct_change.png"), p1, width = 13, height = 9, dpi = 130)
  # box plots of the per-member paired % change (each point = one posterior member)
  write.csv(paired |> filter(period == "all"), file.path(out_dir, "paired_member_changes.csv"), row.names = FALSE)
  bx <- paired |> filter(period == "all", variable %in% KEY_VARS,
                         depth_class != "mid" | variable %in% c("HYD_temp", "CHM_oxy")) |>
    mutate(panel = paste(variable, depth_class, sep = " | "),
           scenario = factor(scenario, c("m1_dairy", "m2_dairy", "m3_dairy",
                                         "m1_non_dairy", "m2_non_dairy", "m3_non_dairy")),
           type = ifelse(grepl("non_dairy", scenario), "non-dairy", "dairy"))
  p_box <- ggplot(bx, aes(scenario, pct, fill = type)) +
    geom_hline(yintercept = 0, colour = "grey40") +
    geom_boxplot(outlier.size = .5, linewidth = .3, width = .7) +
    scale_fill_manual(values = c(dairy = "#4C78A8", `non-dairy` = "#F58518"), name = NULL) +
    facet_wrap(~ panel, scales = "free_y") +
    labs(title = sprintf("%s | change vs baseline per posterior member (n = %d paired members)", lake_tag, length(common)),
         subtitle = "period mean, % of that member's baseline; box = 25-75%, whiskers = 1.5 IQR",
         x = NULL, y = "% change") +
    theme_bw(base_size = 8) + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
  ggsave(file.path(out_dir, "pct_change_boxplot.png"), p_box, width = 13, height = 9, dpi = 130)
  for (v in intersect(c("PHY_tchla", "NIT_tn", "PHS_tp", "CHM_oxy", "NCS_ss1"), unique(ann$variable))) {
    a <- ann |> filter(variable == v, depth_class %in% c("surface", "bottom", "whole_lake"))
    p <- ggplot(a, aes(year, med, colour = scenario, fill = scenario)) +
      geom_ribbon(data = filter(a, scenario == "baseline"), aes(ymin = q05, ymax = q95), alpha = .2, colour = NA) +
      geom_line() + facet_wrap(~ depth_class, scales = "free_y", ncol = 1) +
      labs(title = sprintf("%s | %s annual mean (median; baseline 5-95%% band)", lake_tag, v), x = NULL, y = v) +
      theme_bw(base_size = 9)
    ggsave(file.path(out_dir, sprintf("annual_%s.png", v)), p, width = 9, height = 7, dpi = 130)
  }
  message("[comparison] written to ", out_dir)
  invisible(list(summary = summ, extremes = ext_p, n_members = length(common)))
}

if (sys.nframe() == 0) {
  res <- compare_scenarios(commandArgs(trailingOnly = TRUE)[1])
  show <- res$summary |> filter(period == "all", depth_class %in% c("surface", "whole_lake") | variable == "CHM_oxy",
                                variable %in% KEY_VARS) |>
    transmute(scenario, variable, depth_class, baseline = signif(baseline_mean, 3),
              pct_med = round(pct_med, 1), pct_lo = round(pct_q05, 1), pct_hi = round(pct_q95, 1), robust)
  print(as.data.frame(show), row.names = FALSE)
  print(as.data.frame(res$extremes), row.names = FALSE)
}

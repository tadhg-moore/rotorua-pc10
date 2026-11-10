# In-lake pH distribution, old (2014-2018) and new (2022-) Lake Rotorua buoys.
# Source: calibration/alum/ph_obs.rds (built in calibration/alum/10_ph_explore.R
# from the old_buoy_wq and aeme_buoy_data targets; values outside pH 4-11 removed).
# Output: website/figures/ph_distribution.png for nzfss-2026-alum.qmd

suppressMessages({library(dplyr); library(ggplot2)})

ph <- readRDS(here::here("calibration", "alum", "ph_obs.rds")) |>
  filter(depth <= 5, is.finite(value)) |>
  mutate(src = factor(src, c("old buoy", "new buoy"),
                      c("Old buoy (2014-2018)", "New buoy (2022-2026)")))

stats <- ph |> group_by(src) |>
  summarise(n = n(), med = median(value), p05 = quantile(value, .05),
            p95 = quantile(value, .95), .groups = "drop")
print(stats)

cols <- c("Old buoy (2014-2018)" = "#d95f02", "New buoy (2022-2026)" = "#1b6ca8")

p <- ggplot(ph, aes(value, fill = src, colour = src)) +
  geom_density(alpha = 0.35, linewidth = 0.9, adjust = 1.2) +
  geom_vline(data = stats, aes(xintercept = med, colour = src),
             linetype = 2, linewidth = 0.8, show.legend = FALSE) +
  scale_fill_manual(values = cols) + scale_colour_manual(values = cols) +
  scale_x_continuous(breaks = 5:10, limits = c(5, 9.5)) +
  labs(x = "In-lake pH (surface, ≤ 5 m)", y = "Density", fill = NULL, colour = NULL) +
  theme_minimal(base_size = 18) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        axis.text.y = element_blank(), legend.text = element_text(size = 16))

out <- here::here("website", "figures", "ph_distribution.png")
ggsave(out, p, width = 7, height = 5, dpi = 200, bg = "white")
message("Wrote ", out)

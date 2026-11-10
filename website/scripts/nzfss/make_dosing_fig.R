# Alum dosing history for the two dosed streams (Utuhina, Puarenga).
# Source: alum_dosing target (BOPRC operational records, L/day of alum solution).
# Output: website/figures/alum_dosing.png for nzfss-2026-alum.qmd

suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(lubridate)})

d <- targets::tar_read_raw("alum_dosing", store = here::here("_targets")) |>
  transmute(Date = as.Date(Date), Utuhina = utuhina_alum_l_per_day,
            Puarenga = puarenga_alum_l_per_day) |>
  pivot_longer(-Date, names_to = "stream", values_to = "dose") |>
  mutate(month = floor_date(Date, "month")) |>
  group_by(stream, month) |>
  summarise(dose = mean(dose, na.rm = TRUE), .groups = "drop")

# first month with any dosing, per stream
print(d |> filter(dose > 0) |> group_by(stream) |> summarise(first = min(month), last = max(month)))

cols <- c(Utuhina = "#d95f02", Puarenga = "#1b6ca8")
p <- ggplot(d, aes(month, dose / 1000, colour = stream, fill = stream)) +
  geom_area(alpha = 0.3, position = "identity", linewidth = 0) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", expand = c(0.01, 0)) +
  labs(x = NULL, y = "Alum (1000 L/day)", colour = NULL, fill = NULL) +
  theme_minimal(base_size = 18) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        legend.text = element_text(size = 16))

out <- here::here("website", "figures", "alum_dosing.png")
ggsave(out, p, width = 8, height = 4.6, dpi = 200, bg = "white")
message("Wrote ", out)

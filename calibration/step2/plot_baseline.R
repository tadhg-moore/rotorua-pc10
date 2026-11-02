setwd("C:/Users/mooret/Git/rotorua-pc10"); suppressMessages({library(ggplot2); library(dplyr)})
P <- readRDS("calibration/step2/runs/step2_baseline_pairs.rds")
lab <- c(PHY_tchla = "Chlorophyll-a (ug/L)", PHS_tp = "Total P (g/m3)", PHS_frp = "Filterable reactive P (g/m3)", NIT_tn = "Total N (g/m3)", NIT_nit = "Nitrate (g/m3)", NIT_amm = "Ammonium (g/m3)")
D <- bind_rows(lapply(names(lab), function(v) { d <- P[[v]]; d <- d[d$depth <= 5, ]
  d |> group_by(Date) |> summarise(obs = mean(obs), sim = mean(sim), .groups = "drop") |> mutate(panel = lab[v]) }))
D$panel <- factor(D$panel, lab)
L <- tidyr::pivot_longer(D, c(obs, sim), names_to = "src", values_to = "v")
p <- ggplot(L, aes(Date, v, colour = src)) + geom_line(data = subset(L, src == "sim"), linewidth = 0.3, alpha = .8) + geom_point(data = subset(L, src == "obs"), size = 0.8) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") + scale_colour_manual(values = c(obs = "grey25", sim = "#d95f02"), labels = c(obs = "Observed (surface 0-5 m)", sim = "Model, step 1 parameters"), name = NULL) +
  labs(x = NULL, y = NULL) + theme_minimal(base_size = 10) + theme(legend.position = "top")
dir.create("website/images/step2", showWarnings = FALSE, recursive = TRUE)
ggsave("website/images/step2/baseline_fit.png", p, width = 10, height = 11, dpi = 120, bg = "white")
# obs seasonal/sampling info
d <- P$PHY_tchla; d <- d[d$depth <= 5, ]; cat("chl obs dates per year:\n"); print(table(d$yr |> as.character()) |> as.vector() |> setNames(names(table(d$yr))))

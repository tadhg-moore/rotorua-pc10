# Sensitivity (Morris, sa_latest) against what the joint-stage calibration identified: writes website/data/sensitivity/sens_vs_calib.csv and shortlist csv
suppressMessages({library(dplyr); library(readr)})
r <- read_csv("website/data/sensitivity/morris_ranking.csv", show_col_types = FALSE); j <- read_csv("website/data/calibration/param_summary_j.csv", show_col_types = FALSE)
a <- read_csv("website/data/calibration/param_summary_alum.csv", show_col_types = FALSE)
r <- r |> mutate(rank = row_number(), cum_share = cumsum(phi_share))
cat("n params", nrow(r), " params for 50/80/90% of share:", sapply(c(.5, .8, .9), \(p) which(r$cum_share >= p)[1]), "\n")
cat("negligible (<0.2% share):", sum(r$phi_share < 0.002), " <0.5%:", sum(r$phi_share < 0.005), "\n")
m <- j |> mutate(parameter = parameter) |> inner_join(r |> select(parameter, rank, phi_share, phi_mu_star, phi_lo, phi_hi, phival_scaled, n_targets_top), by = "parameter")
cat("joined", nrow(m), "of", nrow(j), "j params\n"); print(cor(m$shrink, m$phi_share, method = "spearman", use = "complete.obs"))
write_csv(m |> mutate(across(where(is.numeric), \(z) signif(z, 4))), "website/data/sensitivity/sens_vs_calib.csv")
print(as.data.frame(m |> arrange(rank) |> select(parameter, rank, phi_share, shrink, at_bound) |> head(30) |> mutate(across(where(is.numeric), \(z) signif(z, 3)))))
cat("\nsensitive (>1%) but shrink<0.1:\n"); print(as.data.frame(m |> filter(phi_share > 0.01, shrink < 0.1) |> select(parameter, rank, phi_share, shrink)))
cat("\ninsensitive (<0.5%) but shrink>0.3:\n"); print(as.data.frame(m |> filter(phi_share < 0.005, shrink > 0.3) |> select(parameter, rank, phi_share, shrink)))
cat("\nj params ranked outside top 40:", sum(m$rank > 40), "of", nrow(m), "\n")
cat("\nsensitive (>1%) but NOT in j calibration:\n"); print(as.data.frame(r |> filter(phi_share > 0.01, !parameter %in% j$parameter) |> select(parameter, theme, rank, phi_share)))
print(names(a)); print(as.data.frame(a |> head(10)))
# figure: sensitivity vs how much the joint-stage ensemble narrowed
suppressMessages(library(ggplot2))
m$grp <- m$group
p <- ggplot(m, aes(phi_share * 100, shrink * 100, colour = grp)) + geom_point(size = 2) +
  geom_text(data = m |> filter(phi_share > 0.007 | shrink > 0.3), aes(label = parameter), size = 2.6, hjust = -0.1, check_overlap = TRUE, show.legend = FALSE) +
  scale_x_log10(expand = expansion(mult = c(0.03, 0.35))) + scale_colour_brewer(palette = "Dark2") +
  labs(x = "Share of objective sensitivity (%, log axis)", y = "Reduction in ensemble spread, prior to last iteration (%)", colour = NULL) + theme_minimal(base_size = 9) + theme(legend.position = "bottom")
ggsave("website/images/sensitivity/sens_vs_shrink.png", p, width = 7, height = 4.6, dpi = 130, bg = "white")

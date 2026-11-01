# Parameter calibration summary for the joint stage (j) and the alum stage (p4): values by group, convergence by group.
# Reads calibration/step2j/runs/step2_ies.rds and step2_best.rds (j), calibration/step2p4/runs/step2_ies.rds and step2_best.rds (alum). No model runs.
# Outputs: website/data/calibration/param_summary_j.csv, param_summary_group.csv, param_summary_alum.csv; website/images/calibration/param_conv_<group>.png, group_convergence.png, alum_conv.png
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(readr); library(patchwork)})
out <- "website/images/calibration"; dat <- "website/data/calibration"; dir.create(out, recursive = TRUE, showWarnings = FALSE); dir.create(dat, recursive = TRUE, showWarnings = FALSE)
th <- theme_minimal(base_size = 9) + theme(plot.title = element_text(size = 8, face = "bold"), axis.title = element_blank())
# natural value from a position u in [0, 1] (same rule as step2_ies.R)
val <- function(u, r) if (identical(r$scale, "log") && r$min * r$max > 0) { lo <- log(min(abs(r$min), abs(r$max))); hi <- log(max(abs(r$min), abs(r$max))); sign(r$min) * exp(lo + u * (hi - lo)) } else r$min + u * (r$max - r$min)
nat <- function(U, v) sapply(seq_len(ncol(U)), function(j) val(U[, j], v[j, ]))
labs_of <- function(v) paste0(sub("^aed_[a-z_0-9]+/", "", v$name), ifelse(is.na(v$group), ifelse(is.na(v$index), "", paste0(" z", v$index)), paste0(" ", v$group)))
GROUPS <- c(phyto = "Phytoplankton P kinetics", oxygen = "Oxygen and organic matter", phosphorus = "Sediment P release", nitrogen = "Nitrogen")
signif_t <- function(x) ifelse(abs(x) >= 1000 | abs(x) < 0.001, formatC(x, format = "e", digits = 2), formatC(signif(x, 3), format = "fg", digits = 3))

# ---------------- joint stage ----------------
x <- readRDS("calibration/step2j/runs/step2_ies.rds"); b <- readRDS("calibration/step2j/runs/step2_best.rds"); H <- x$hist; nit <- length(H)
v <- x$tab[x$vary, ]; v$label <- labs_of(v); v$theme <- ifelse(v$theme == "joint", "other", v$theme)
okU <- function(i) H[[i]]$U[H[[i]]$ok, , drop = FALSE]
Nat <- lapply(seq_len(nit), function(i) nat(okU(i), v))
bi <- which.min(H[[nit]]$phi); Ubest <- b$U_best
tab <- tibble(group = unname(GROUPS[v$theme]), theme = v$theme, parameter = v$label, scale = v$scale, prior_min = v$min, prior_max = v$max, start = v$value,
              best = as.numeric(nat(matrix(Ubest, 1), v)), final_q10 = sapply(Nat[[nit]] |> as.data.frame() |> as.list(), quantile, .1), final_median = sapply(as.data.frame(Nat[[nit]]), median), final_q90 = sapply(as.data.frame(Nat[[nit]]), quantile, .9),
              pos_best = Ubest, pos_median = apply(okU(nit), 2, median), shrink = 1 - apply(okU(nit), 2, sd) / apply(H[[1]]$U, 2, sd)) |>
  mutate(at_bound = ifelse(pos_best < 0.05, "lower", ifelse(pos_best > 0.95, "upper", "")))
# negative fluxes (fsed_oxy, fsed_nit) are stored as negative numbers: report the magnitude so the table reads in the same direction as the range
tab <- tab |> mutate(across(c(prior_min, prior_max, start, best, final_q10, final_median, final_q90), \(z) ifelse(grepl("fsed_oxy|fsed_nit", parameter), abs(z), z)))
tab <- tab |> mutate(lo = pmin(prior_min, prior_max), hi = pmax(prior_min, prior_max), prior_min = lo, prior_max = hi) |> select(-lo, -hi)
tab <- tab |> mutate(across(c(final_q10, final_q90), \(z) z)) |> mutate(q_lo = pmin(final_q10, final_q90), q_hi = pmax(final_q10, final_q90), final_q10 = q_lo, final_q90 = q_hi) |> select(-q_lo, -q_hi)
write_csv(tab |> mutate(across(where(is.numeric), \(z) signif(z, 4))), file.path(dat, "param_summary_j.csv"))
grp <- tab |> group_by(group) |> summarise(n = n(), median_shrink = median(shrink), n_shrink_gt_30 = sum(shrink > 0.3), n_best_at_bound = sum(at_bound != ""), n_moved_gt_025 = sum(abs(pos_median - 0.5) > 0.25 & shrink > 0.1), .groups = "drop")
write_csv(grp |> mutate(across(where(is.numeric), \(z) round(z, 3))), file.path(dat, "param_summary_group.csv")); print(as.data.frame(grp))
print(as.data.frame(tab |> mutate(across(where(is.numeric), \(z) signif(z, 3))) |> select(group, parameter, prior_min, prior_max, start, best, final_median, shrink, at_bound)), row.names = FALSE)

# ---- convergence per group: one panel per parameter, natural units, median and 10-90% band ----
cols <- c(phyto = "#1b9e77", oxygen = "#d95f02", phosphorus = "#7570b3", nitrogen = "#e7298a")
panel <- function(j) { N <- lapply(Nat, \(m) { z <- m[, j]; if (grepl("fsed_oxy|fsed_nit", v$label[j])) abs(z) else z })
  d <- tibble(iter = 0:(nit - 1), lo = sapply(N, quantile, .1), med = sapply(N, median), hi = sapply(N, quantile, .9))
  rng <- c(tab$prior_min[j], tab$prior_max[j]); lg <- v$scale[j] == "log" && all(rng > 0)
  p <- ggplot(d, aes(iter)) + geom_hline(yintercept = rng, colour = "grey60", linetype = "dotted") + geom_hline(yintercept = tab$start[j], colour = "grey40", linetype = "dashed", linewidth = 0.3) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = cols[[v$theme[j]]], alpha = 0.3) + geom_line(aes(y = med), colour = cols[[v$theme[j]]]) +
    annotate("point", x = nit - 1, y = tab$best[j], colour = "firebrick", size = 1.6) + scale_x_continuous(breaks = c(0, 3, 6)) + labs(title = v$label[j]) + th
  if (lg) p + scale_y_log10() else p }
for (g in names(GROUPS)) { js <- which(v$theme == g); ps <- lapply(js, panel); nc <- if (length(js) > 9) 5 else 4
  ggsave(file.path(out, paste0("param_conv_", g, ".png")), wrap_plots(ps, ncol = nc) + plot_annotation(subtitle = "Median and 10 to 90% of the ensemble by iteration (0 = prior). Dotted: allowed range. Dashed: start member. Red point: best member.", theme = theme(plot.subtitle = element_text(size = 8))),
         width = 11, height = 1.9 * ceiling(length(js) / nc) + 0.6, dpi = 130, bg = "white") }

# ---- group-level convergence: normalised spread and objective ----
sp <- bind_rows(lapply(seq_len(nit), function(i) tibble(iter = i - 1, theme = v$theme, sd = apply(H[[i]]$U[H[[i]]$ok, , drop = FALSE], 2, sd), sd0 = apply(H[[1]]$U, 2, sd)))) |>
  group_by(iter, theme) |> summarise(rel_spread = median(sd / sd0), .groups = "drop") |> mutate(group = factor(GROUPS[theme], GROUPS))
phi <- bind_rows(lapply(seq_len(nit), function(i) { p <- H[[i]]$phi[H[[i]]$ok]; tibble(iter = i - 1, best = min(p), q25 = quantile(p, .25), median = median(p), q75 = quantile(p, .75)) }))
write_csv(sp |> mutate(rel_spread = round(rel_spread, 3)), file.path(dat, "param_group_spread.csv")); write_csv(phi |> mutate(across(where(is.numeric), \(z) signif(z, 4))), file.path(dat, "param_phi_j.csv")); print(as.data.frame(sp |> pivot_wider(names_from = theme, values_from = rel_spread, id_cols = iter)))
g1 <- ggplot(sp, aes(iter, rel_spread, colour = theme)) + geom_line(linewidth = 0.9) + geom_point(size = 1.6) + scale_colour_manual(values = cols, labels = GROUPS) + scale_x_continuous(breaks = 0:(nit - 1)) + coord_cartesian(ylim = c(0, 1.05)) +
  labs(title = "Ensemble spread by group", subtitle = "Median SD relative to the prior (1 = unchanged)", x = "Iteration", y = NULL, colour = NULL) + theme_minimal(base_size = 9) + theme(legend.position = "bottom")
g2 <- ggplot(phi, aes(iter)) + geom_ribbon(aes(ymin = q25, ymax = q75), fill = "grey70", alpha = 0.5) + geom_line(aes(y = median, linetype = "Median (band: interquartile range)")) + geom_line(aes(y = best, linetype = "Best member"), colour = "firebrick") +
  scale_y_log10() + scale_x_continuous(breaks = 0:(nit - 1)) + labs(title = "Objective by iteration", subtitle = "Calibration-window objective, log axis", x = "Iteration", y = NULL, linetype = NULL) + theme_minimal(base_size = 9) + theme(legend.position = "bottom")
ggsave(file.path(out, "group_convergence.png"), g1 + g2, width = 10, height = 4, dpi = 130, bg = "white")

# ---------------- alum stage (p4) ----------------
xa <- readRDS("calibration/step2p4/runs/step2_ies.rds"); ba <- readRDS("calibration/step2p4/runs/step2_best.rds"); Ha <- xa$hist; na <- length(Ha); va <- xa$tab[xa$vary, ]; va$label <- labs_of(va)
alum <- va$theme == "alum"; cat("p4 varied:", va$label, "| iterations", na, "\n")
Na <- lapply(seq_len(na), function(i) nat(Ha[[i]]$U[Ha[[i]]$ok, , drop = FALSE], va)); Ua <- Ha[[na]]$U[Ha[[na]]$ok, , drop = FALSE]
ta <- tibble(parameter = va$label, prior_min = va$min, prior_max = va$max, start = va$value, best = as.numeric(nat(matrix(ba$U_best, 1), va)), final_q10 = sapply(as.data.frame(Na[[na]]), quantile, .1), final_median = sapply(as.data.frame(Na[[na]]), median), final_q90 = sapply(as.data.frame(Na[[na]]), quantile, .9),
             pos_best = ba$U_best, shrink = 1 - apply(Ua, 2, sd) / apply(Ha[[1]]$U, 2, sd)) |> mutate(at_bound = ifelse(pos_best < 0.05, "lower", ifelse(pos_best > 0.95, "upper", "")))
write_csv(ta |> mutate(across(where(is.numeric), \(z) signif(z, 4))), file.path(dat, "param_summary_alum.csv")); print(as.data.frame(ta |> mutate(across(where(is.numeric), \(z) signif(z, 3)))), row.names = FALSE)
pa <- lapply(seq_len(nrow(va)), function(j) { d <- tibble(iter = 0:(na - 1), lo = sapply(Na, \(m) quantile(m[, j], .1)), med = sapply(Na, \(m) median(m[, j])), hi = sapply(Na, \(m) quantile(m[, j], .9)))
  ggplot(d, aes(iter)) + geom_hline(yintercept = c(va$min[j], va$max[j]), colour = "grey60", linetype = "dotted") + geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#4d4d4d", alpha = 0.3) + geom_line(aes(y = med)) +
    annotate("point", x = na - 1, y = ta$best[j], colour = "firebrick", size = 1.6) + scale_x_continuous(breaks = 0:(na - 1)) + scale_y_log10() + labs(title = va$label[j]) + th })
ggsave(file.path(out, "alum_conv.png"), wrap_plots(pa, ncol = 4), width = 11, height = 1.9 * ceiling(length(pa) / 4) + 0.3, dpi = 130, bg = "white")
cat("done\n")

# ---------------- parameters held fixed in the joint stage (set by the earlier stages) ----------------
fx <- x$tab; fx$label <- labs_of(fx); fx <- fx[fx$theme == "fixed", ]
fx$class <- ifelse(fx$file %in% c("met", "glm3.nml", "glm4.nml"), "Physics (stages 1 and 2)", ifelse(fx$name %in% c("aed_organic_matter/w_pom", "aed_organic_matter/rpop_hydrol", "aed_phosphorus/w_po4ads", "aed_phosphorus/kpo4p"), "Particulate P (stage q and refits)", "Other biogeochemistry"))
write_csv(fx |> transmute(class, parameter = sub("^(aed_[a-z_0-9]+|[a-z]+)/", "", label), file, value = signif(value, 4)), file.path(dat, "param_summary_fixed.csv"))
print(table(fx$class))

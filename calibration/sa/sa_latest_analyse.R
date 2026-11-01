# Analysis of the Morris sensitivity run on the latest setup (calibration/sa/sa_latest.R). Works on whatever trajectories are complete.
#   Rscript calibration/sa/sa_latest_analyse.R
# Elementary effect of parameter j in a step: (Y_after - Y_before) / (+-delta), parameters on the [0, 1] range scale, so mu* is the mean absolute change in the output over the full parameter range.
# mu*_scaled = mu* / SD of the output over all successful runs (comparable across outputs). Bootstrap over trajectories (B = 1000) gives the interval of mu*.
# Outputs: website/data/sensitivity/*.csv and website/images/sensitivity/*.png
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(readr)})
dir <- "calibration/sa_latest"; out_d <- "website/data/sensitivity"; out_i <- "website/images/sensitivity"; for (d in c(out_d, out_i)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
D <- readRDS(file.path(dir, "sa_design.rds")); design <- D$design; tab <- D$tab; vary <- D$vary; delta <- D$delta; k <- length(vary)
lab <- paste0(sub("^aed_[a-z_0-9]+/|^[a-z]+/", "", tab$name[vary]), ifelse(is.na(tab$group[vary]), ifelse(is.na(tab$index[vary]), "", paste0(" z", tab$index[vary])), paste0(" ", tab$group[vary])))
lab[tab$name[vary] == "aed_sed_const2d/fsed_nit"] <- paste0("fsed_nit z", tab$index[vary][tab$name[vary] == "aed_sed_const2d/fsed_nit"])
theme_p <- ifelse(is.na(tab$theme[vary]), "other", tab$theme[vary]); lab <- make.unique(lab, sep = " #")
files <- file.path(dir, "results", sprintf("run_%05d.rds", design[, "run"])); have <- file.exists(files); cat(sum(have), "of", nrow(design), "runs finished\n")
R <- lapply(files[have], readRDS); idx <- design[have, "run"]
nm <- unique(unlist(lapply(R, function(r) names(r$out)))); Y <- matrix(NA_real_, nrow(design), length(nm), dimnames = list(NULL, nm))
for (r in R) if (!is.null(r$out)) Y[r$run, names(r$out)] <- r$out
# the raw objective has blow-up runs (collapsed phytoplankton gives residuals in the hundreds), so rank on log10 of it; the raw version stays in the effects table
Y <- cbind(Y, phil_cal = log10(Y[, "phi_cal"]), phil_val = log10(Y[, "phi_val"])); Y[!is.finite(Y)] <- NA
fail <- have & !vapply(seq_len(nrow(design)), function(i) i %in% idx && any(is.finite(Y[i, ])), logical(1)); cat(sum(fail), "failed runs\n")
secs <- vapply(R, function(r) r$secs, 1); cat("median run time (min):", round(median(secs) / 60, 2), "\n")
ok_out <- colSums(is.finite(Y)) > 0.5 * sum(have) & apply(Y, 2, function(y) sd(y, na.rm = TRUE) > 0); Y <- Y[, ok_out, drop = FALSE]; cat(ncol(Y), "outputs with variation\n")
sdY <- apply(Y, 2, sd, na.rm = TRUE)
# ---- elementary effects: array [trajectory, parameter, output] ----
traj_ids <- sort(unique(design[, "traj"])); EE <- array(NA_real_, c(length(traj_ids), k, ncol(Y)))
for (ti in seq_along(traj_ids)) { rows <- which(design[, "traj"] == traj_ids[ti]); if (!all(have[rows])) next
  for (s in 2:length(rows)) { j <- design[rows[s], "param"]; d <- design[rows[s], -(1:4)][j] - design[rows[s - 1], -(1:4)][j]
    EE[ti, j, ] <- (Y[rows[s], ] - Y[rows[s - 1], ]) / d } }
done <- which(apply(EE, 1, function(m) any(is.finite(m)))); EE <- EE[done, , , drop = FALSE]; cat(length(done), "complete trajectories\n")
stat <- function(E) { n <- apply(E, c(2, 3), function(x) sum(is.finite(x))); list(mu = apply(E, c(2, 3), mean, na.rm = TRUE), mustar = apply(abs(E), c(2, 3), mean, na.rm = TRUE), sigma = apply(E, c(2, 3), sd, na.rm = TRUE), n = n) }
S <- stat(EE); set.seed(1); B <- 1000; nt <- dim(EE)[1]
bs <- array(NA_real_, c(B, k, ncol(Y))); for (b in seq_len(B)) { ii <- sample(nt, nt, replace = TRUE); bs[b, , ] <- apply(abs(EE[ii, , , drop = FALSE]), c(2, 3), mean, na.rm = TRUE) }
lo <- apply(bs, c(2, 3), quantile, 0.025, na.rm = TRUE); hi <- apply(bs, c(2, 3), quantile, 0.975, na.rm = TRUE)
long <- expand.grid(i = seq_len(k), o = seq_len(ncol(Y))) |> transmute(parameter = lab[i], theme = theme_p[i], output = colnames(Y)[o], mu = S$mu[cbind(i, o)], mu_star = S$mustar[cbind(i, o)], sigma = S$sigma[cbind(i, o)], mu_star_lo = lo[cbind(i, o)], mu_star_hi = hi[cbind(i, o)], n = S$n[cbind(i, o)], mu_star_scaled = S$mustar[cbind(i, o)] / sdY[o], lo_scaled = lo[cbind(i, o)] / sdY[o], hi_scaled = hi[cbind(i, o)] / sdY[o])
cls <- function(o) case_when(grepl("^cal_", o) ~ "target (calibration window)", grepl("^val_", o) ~ "target (held-out window)", grepl("^phi_", o) ~ "objective", grepl("^sig_", o) ~ "CSPS signature", grepl("^st_", o) ~ "CSPS state error", grepl("^x_", o) ~ "physical summary", TRUE ~ "other")
long$class <- cls(long$output); write_csv(long |> mutate(across(where(is.numeric), \(z) signif(z, 4))), file.path(out_d, "morris_effects.csv"))
cat("written morris_effects.csv:", nrow(long), "rows\n")
# ---- parameter ranking on the objective and on the targets ----
tg <- grep("^cal_", colnames(Y), value = TRUE)
rank_tab <- long |> filter(output %in% tg) |> group_by(parameter, theme) |> summarise(n_targets_top = sum(mu_star_scaled > 0.25 & lo_scaled > 0.1, na.rm = TRUE), max_target = max(mu_star_scaled, na.rm = TRUE), mean_target = mean(mu_star_scaled, na.rm = TRUE), .groups = "drop") |>
  left_join(long |> filter(output == "phil_cal") |> select(parameter, phi_mu_star = mu_star, phi_lo = mu_star_lo, phi_hi = mu_star_hi, phi_sigma = sigma, phi_mu = mu, phi_scaled = mu_star_scaled), by = "parameter") |>
  left_join(long |> filter(output == "phil_val") |> select(parameter, phival_mu_star = mu_star, phival_scaled = mu_star_scaled), by = "parameter") |> arrange(desc(phi_mu_star))
rank_tab$phi_share <- rank_tab$phi_mu_star / sum(rank_tab$phi_mu_star); write_csv(rank_tab |> mutate(across(where(is.numeric), \(z) signif(z, 4))), file.path(out_d, "morris_ranking.csv"))
cat("top 25 by mu* of the calibration objective:\n"); print(as.data.frame(rank_tab[1:25, c("parameter", "theme", "phi_mu_star", "phi_lo", "phi_hi", "phi_sigma", "phi_share", "n_targets_top")] |> mutate(across(where(is.numeric), \(z) signif(z, 3)))), row.names = FALSE)
grp <- rank_tab |> group_by(theme) |> summarise(n = n(), share_of_phi = sum(phi_share), n_over_1pct = sum(phi_share > 0.01), n_negligible = sum(phi_share < 0.002), .groups = "drop") |> arrange(desc(share_of_phi)); write_csv(grp |> mutate(across(where(is.numeric), \(z) signif(z, 3))), file.path(out_d, "morris_groups.csv")); print(as.data.frame(grp))
# ---- convergence: rank agreement of mu* with the full set, as trajectories are added ----
conv <- bind_rows(lapply(unique(pmin(c(5, 10, 15, 20, 25, nt), nt)), function(r) { Sr <- apply(abs(EE[seq_len(r), , , drop = FALSE]), c(2, 3), mean, na.rm = TRUE)
  op <- match("phil_cal", colnames(Y)); data.frame(trajectories = r, rho_phi = cor(Sr[, op], S$mustar[, op], method = "spearman", use = "complete.obs"), top10_overlap_phi = length(intersect(order(-Sr[, op])[1:10], order(-S$mustar[, op])[1:10])),
    rho_targets = mean(sapply(tg, function(t) cor(Sr[, match(t, colnames(Y))], S$mustar[, match(t, colnames(Y))], method = "spearman", use = "complete.obs")), na.rm = TRUE)) }))
write_csv(conv |> mutate(across(where(is.numeric), \(z) round(z, 3))), file.path(out_d, "morris_convergence.csv")); print(conv)
# ---- figures ----
th <- theme_minimal(base_size = 9) + theme(legend.position = "bottom")
tcol <- c(phytoplankton = "#1b9e77", physics = "#7570b3", alum = "#e7298a", sediment = "#d95f02", `organic matter` = "#a6761d", nitrogen = "#66a61e", phosphorus = "#e6ab02", light = "#666666", other = "grey50")
top <- rank_tab |> slice_head(n = 40) |> mutate(parameter = factor(parameter, rev(parameter)))
ggsave(file.path(out_i, "phi_ranking.png"), ggplot(top, aes(parameter, phi_mu_star, fill = theme)) + geom_col() + geom_errorbar(aes(ymin = phi_lo, ymax = phi_hi), width = 0.3, linewidth = 0.3) + coord_flip() + scale_fill_manual(values = tcol) + labs(x = NULL, y = "mu* of log10 calibration objective (decades over the full parameter range), 95% bootstrap interval", fill = NULL) + th, width = 7.5, height = 8, dpi = 130, bg = "white")
ph <- long |> filter(output == "phil_cal", mu_star > 0, sigma > 0); ggsave(file.path(out_i, "phi_mu_sigma.png"), ggplot(ph, aes(mu_star, sigma, colour = theme)) + geom_abline(slope = 1, colour = "grey70", linetype = "dashed") + geom_point(size = 1.4) + geom_text(data = ph |> slice_max(mu_star, n = 14), aes(label = parameter), size = 2.4, hjust = -0.1, check_overlap = TRUE, show.legend = FALSE) + scale_colour_manual(values = tcol) + scale_x_log10(expand = expansion(mult = c(0.03, 0.3))) + scale_y_log10(expand = expansion(mult = c(0.03, 0.1))) + labs(x = "mu* (log)", y = "sigma, interaction / non-linearity (log)", colour = NULL, subtitle = "Calibration objective. Points above the dashed line: effect varies more than it averages (interactions or non-linearity)") + th, width = 7, height = 5.5, dpi = 130, bg = "white")
keyt <- long |> filter(output %in% tg) |> mutate(parameter = factor(parameter, rev(rank_tab$parameter)), output = sub("^cal_", "", output)); keyt <- keyt |> filter(parameter %in% rank_tab$parameter[1:45])
ggsave(file.path(out_i, "target_heatmap.png"), ggplot(keyt, aes(output, parameter, fill = pmin(mu_star_scaled, 1.5))) + geom_tile() + scale_fill_viridis_c(option = "magma", direction = -1, name = "mu* / SD of output (capped at 1.5)") + labs(x = NULL, y = NULL, subtitle = "Top 45 parameters by objective sensitivity; calibration-window targets") + th + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)), width = 9, height = 9, dpi = 130, bg = "white")
cat("done\n")

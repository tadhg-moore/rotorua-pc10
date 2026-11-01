# Joint stage (j): parameter convergence, prior-to-posterior shrinkage, how each target pulled each parameter, and target-by-target change.
# Reads calibration/step2j/runs/step2_ies.rds (hist: per-iteration U, Ycal, phi). No model runs.
#   pull_ij = -beta_ij * y_i(start member), beta from a regression of target i on the parameters over the first iterations; positive = the target wants a higher parameter value
#   (the ensemble smoother update with one target and no noise); positive = move towards the upper bound.
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(readr)})
out <- "website/images/calibration"; dat <- "website/data/calibration"; dir.create(out, recursive = TRUE, showWarnings = FALSE); dir.create(dat, recursive = TRUE, showWarnings = FALSE)
th <- theme_minimal(base_size = 9) + theme(legend.position = "bottom", strip.text = element_text(face = "bold", size = 7))
sv <- function(n, p, w, h) ggsave(file.path(out, n), p, width = w, height = h, dpi = 130, bg = "white")
dir <- Sys.getenv("J_DIR", "calibration/step2j/runs")
x <- readRDS(file.path(dir, "step2_ies.rds")); H <- x$hist; v <- x$tab[x$vary, ]; nit <- length(H); cat("iterations stored:", nit, "\n")
lab <- paste0(sub("^aed_[a-z_0-9]+/", "", v$name), ifelse(is.na(v$group), ifelse(is.na(v$index), "", paste0(" z", v$index)), paste0(" ", v$group)))
theme_of <- ifelse(v$theme == "joint", "other", v$theme); lab <- factor(lab, unique(lab[order(factor(theme_of, c("phyto", "oxygen", "phosphorus", "nitrogen", "other")))]))
pdat <- data.frame(par = lab, theme = theme_of)
# ---- convergence of the ensemble in each parameter ----
C <- bind_rows(lapply(seq_len(nit), function(i) { U <- H[[i]]$U[H[[i]]$ok, , drop = FALSE]; tibble(iter = i - 1, par = lab, lo = apply(U, 2, quantile, .1), med = apply(U, 2, median), hi = apply(U, 2, quantile, .9)) })) |> left_join(distinct(pdat), by = "par")
sv("joint_convergence.png", ggplot(C, aes(iter)) + geom_ribbon(aes(ymin = lo, ymax = hi, fill = theme), alpha = 0.3) + geom_line(aes(y = med, colour = theme)) + facet_wrap(~par, ncol = 8) + coord_cartesian(ylim = c(0, 1)) +
     scale_x_continuous(breaks = c(0, 3, 6)) + labs(x = "Iteration (0 = prior ensemble)", y = "Position in allowed range (median, 10-90%)", fill = NULL, colour = NULL, subtitle = "A band that narrows and settles has been constrained by the targets; a band that stays wide is not identified") + th, 12, 9)
# ---- shrinkage ----
sd0 <- apply(H[[1]]$U, 2, sd); sd1 <- apply(H[[nit]]$U[H[[nit]]$ok, , drop = FALSE], 2, sd); sh <- tibble(par = lab, shrink = 1 - sd1 / sd0) |> left_join(distinct(pdat), by = "par")
write_csv(sh |> mutate(shrink = round(shrink, 3)), file.path(dat, "joint_shrinkage.csv"))
sv("joint_shrinkage.png", ggplot(sh, aes(reorder(par, shrink), shrink, fill = theme)) + geom_col() + coord_flip() + labs(x = NULL, y = "Reduction in ensemble spread from prior to final iteration (1 = fully constrained, 0 = unchanged)", fill = NULL) + th, 7, 7.5)
# ---- target pull ----
# Local sensitivity of every target to every parameter: linear regression of the target on the parameters (range units), pooled over the first four iterations (about 400 members);
# the pull of target i on parameter j is the downhill direction of that target's squared residual at the start member: -beta_ij * y_i(start). Links with |t| < 3 are blank (not identified).
POOL <- seq_len(min(4, nit)); Up <- do.call(rbind, lapply(POOL, function(i) H[[i]]$U[H[[i]]$ok, , drop = FALSE])); Yp <- do.call(rbind, lapply(POOL, function(i) H[[i]]$Ycal[H[[i]]$ok, , drop = FALSE]))
y0 <- H[[1]]$Ycal[1, ]; ok <- apply(Yp, 2, function(y) sd(y, na.rm = TRUE) > 1e-6 & all(is.finite(y)))
colnames(Up) <- paste0("p", seq_len(ncol(Up)))
Pm <- t(sapply(which(ok), function(i) { f <- summary(lm(Yp[, i] ~ Up)); cf <- f$coefficients[-1, , drop = FALSE]; ifelse(abs(cf[, "t value"]) >= 3, -cf[, "Estimate"] * y0[i], 0) })); rownames(Pm) <- colnames(Yp)[ok]; colnames(Pm) <- as.character(lab)
Pm <- Pm / max(abs(Pm), na.rm = TRUE)
U0 <- H[[1]]$U[H[[1]]$ok, , drop = FALSE]
grp <- function(n) dplyr::case_when(grepl("^TP|^FRP|^Al", n) ~ "Phosphorus", grepl("^chl", n) ~ "Chlorophyll", grepl("^TN|^NIT", n) ~ "Nitrogen", grepl("^do_|^oxy|^anox|^domin|^doslope", n) ~ "Oxygen", grepl("^temp|^thermo", n) ~ "Temperature and thermocline", TRUE ~ "Other")
PL <- as.data.frame(Pm) |> tibble::rownames_to_column("target") |> pivot_longer(-target, names_to = "par", values_to = "pull") |> mutate(par = factor(par, levels(lab)), group = grp(target), target = factor(target, rownames(Pm)))
write_csv(PL |> mutate(pull = round(pull, 4)), file.path(dat, "joint_target_pull.csv"))
PL <- PL |> group_by(target) |> mutate(pullc = pull / pmax(max(abs(pull)), 1e-12)) |> ungroup(); lim <- 1   # each target scaled to its strongest parameter
net <- tibble(par = lab, net = colMeans(H[[nit]]$U[H[[nit]]$ok, , drop = FALSE]) - colMeans(U0))
sv("joint_target_pull.png", ggplot(PL, aes(target, par, fill = pullc)) + geom_tile() + facet_grid(. ~ group, scales = "free_x", space = "free_x") + scale_y_discrete(limits = rev(levels(lab))) +
     scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac", limits = c(-lim, lim), name = "Relative pull (within target)") + labs(x = NULL, y = NULL, subtitle = "Blue: reducing this target residual asks for a higher parameter value; red: a lower value. Blank: no identified link (|t| < 3)") + th + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6), panel.spacing.x = unit(0.2, "lines")), 13, 8)
sv("joint_net_shift.png", ggplot(net |> left_join(distinct(pdat), by = "par"), aes(reorder(par, net), net, fill = net > 0)) + geom_col() + coord_flip() + scale_fill_manual(values = c("#b2182b", "#2166ac"), guide = "none") + labs(x = NULL, y = "Net shift of the ensemble mean, prior to final (range units)") + th, 6, 7.5)
# dominant targets per parameter (for the text)
top <- PL |> group_by(par) |> arrange(desc(abs(pull)), .by_group = TRUE) |> slice_head(n = 3) |> summarise(drivers = paste0(target, " (", ifelse(pull > 0, "+", "-"), sprintf("%.2f", abs(pull)), ")", collapse = "; "), .groups = "drop") |> left_join(net, by = "par")
write_csv(top |> mutate(net = round(net, 3)), file.path(dat, "joint_parameter_drivers.csv")); print(as.data.frame(top), row.names = FALSE)
# ---- target by target: start member (o3) against the best member ----
bi <- which.min(H[[nit]]$phi); T <- tibble(target = colnames(H[[1]]$Ycal), start = H[[1]]$Ycal[1, ], best = H[[nit]]$Ycal[bi, ], prior_median = apply(H[[1]]$Ycal, 2, median), group = grp(colnames(H[[1]]$Ycal)))
write_csv(T |> mutate(across(where(is.numeric), \(z) round(z, 3))), file.path(dat, "joint_target_residuals.csv"))
sv("joint_target_change.png", ggplot(T |> pivot_longer(c(start, best), names_to = "member", values_to = "resid") |> mutate(target = factor(target, T$target), member = factor(member, c("start", "best"), c("start (o3 member)", "best joint member"))), aes(resid, target, colour = member)) + geom_vline(xintercept = 0, colour = "grey60") + geom_point(size = 1.6) +
     facet_grid(group ~ ., scales = "free_y", space = "free_y") + scale_colour_manual(values = c("grey40", "firebrick")) + labs(x = "Scaled residual, calibration window (0 = perfect; values are in units of the target's tolerance)", y = NULL, colour = NULL) + th, 8, 9)
cat("done\n"); cat("phi start", round(H[[1]]$phi[1], 3), "best", round(H[[nit]]$phi[bi], 3), "\n")

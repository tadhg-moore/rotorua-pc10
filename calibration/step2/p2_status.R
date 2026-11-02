x <- readRDS("calibration/step2p2/runs/step2_ies.rds"); b <- readRDS("calibration/step2p2/runs/step2_best.rds")
cat("phi: start", round(b$baseline_phi, 3), "-> best", round(b$phi_best, 3), "\n")
r <- cbind(start_cal = b$baseline_Ycal, best_cal = b$best_Ycal, start_val = b$baseline_Yval, best_val = b$best_Yval); print(round(r, 2))
v <- x$tab[x$vary, c("name", "value", "min", "max", "scale")]; to_value <- function(u, r) { lo <- log(min(abs(r$min), abs(r$max))); hi <- log(max(abs(r$min), abs(r$max))); sign(r$min) * exp(lo + u * (hi - lo)) }
cat("\nbest member parameters (start -> best; range):\n"); for (j in seq_along(x$vary)) cat(sprintf("  %-34s %9.4f -> %9.4f   [%g, %g]\n", v$name[j], v$value[j], to_value(b$U_best[j], v[j, ]), v$min[j], v$max[j]))
h <- x$hist[[length(x$hist)]]; ok <- h$phi < quantile(h$phi, 0.25, na.rm = TRUE)
cat("\nfinal ensemble, top quarter (n =", sum(ok), ") parameter range:\n"); for (j in seq_along(x$vary)) { vv <- vapply(which(ok), function(i) to_value(h$U[i, j], v[j, ]), 1); cat(sprintf("  %-34s %9.4f to %9.4f (median %9.4f)\n", v$name[j], min(vv), max(vv), median(vv))) }

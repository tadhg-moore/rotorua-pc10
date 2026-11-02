# Compare the three lake alum refits (14 free parameters: 8 alum + 6 sediment P; joint-stage objective; start from the stage j best member).
# H1b = Puarenga thesis stream rule, beta 1; H1b_B143 = same with lake capacity 1.43 x Al (beta); H0 = previous (ramp) stream rule, beta 1.
# Objective: phi = mean of squared scaled residuals (calibration window 2006-06 to 2018-06); validation = same on 2018-07 to 2022-06.
suppressMessages({library(dplyr); library(tidyr); library(readr)})
toval <- function(u, mn, mx, sc) if (sc == "log") exp(log(mn) + u * (log(mx) - log(mn))) else mn + u * (mx - mn)
runs <- c(H0 = "H0 (previous ramp)", H1b = "H1b (thesis, Puarenga)", H1b_B143 = "H1b + beta 1.43")
P <- list(); S <- list()
for (t in names(runs)) { r <- readRDS(sprintf("calibration/step2h1_%s/runs/step2_ies.rds", t)); L <- r$hist[[length(r$hist)]]; B <- r$hist[[1]]; tb <- r$tab[r$vary, ]
  ok <- L$ok; bi <- which.min(ifelse(ok, L$phi, Inf)); vphi <- rowMeans(L$Yval^2)
  S[[t]] <- tibble(run = runs[t], phi_start_cal = B$phi[1], phi_start_val = mean(B$Yval[1, ]^2), phi_best_cal = L$phi[bi], phi_best_val = vphi[bi], phi_med_cal = median(L$phi[ok]), phi_med_val = median(vphi[ok]))
  V <- sapply(seq_len(nrow(tb)), function(j) toval(L$U[ok, j], tb$min[j], tb$max[j], tb$scale[j]))
  P[[t]] <- tibble(run = runs[t], param = sub(".*/", "", tb$name), group = ifelse(grepl("fsed_frp", tb$name), paste0("zone", tb$index), NA), start = tb$value, best = V[which.min(L$phi[ok]), ], med = apply(V, 2, median), p10 = apply(V, 2, quantile, 0.1), p90 = apply(V, 2, quantile, 0.9), lo = tb$min, hi = tb$max,
                   frac_range_p10_p90 = (log(apply(V, 2, quantile, 0.9)) - log(apply(V, 2, quantile, 0.1))) / (log(tb$max) - log(tb$min))) }
S <- bind_rows(S); P <- bind_rows(P); dir.create("website/data/h1", FALSE, TRUE); write_csv(S, "website/data/h1/refit_summary.csv"); write_csv(P, "website/data/h1/refit_params.csv")
print(as.data.frame(S), digits = 3); P |> mutate(across(where(is.numeric), \(x) signif(x, 3))) |> select(run, param, group, start, best, med, p10, p90, lo, hi) |> arrange(param, run) |> as.data.frame() |> print()

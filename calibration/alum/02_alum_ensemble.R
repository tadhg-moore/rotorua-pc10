# Alum-parameter ensemble: the best quarter of the final alum-calibration ensemble (round 1), each run alum ON and alum OFF over 2005-2022
# on the O member (all other parameters fixed), to show how much the unconstrained alum parameters matter.
#   Rscript calibration/alum/02_alum_ensemble.R      (AL_FROM = calibration/step2alum/runs, AL_FRAC = 0.25)
Sys.setenv(S2_STAGE = "o", S2_DIR = file.path("calibration", "alum", "ens_runs"), S2_ALUM = "1", S2_BASE = "calibration/step3/aeme_base_2005_alum.rds",
           S2_ALUM_FROM = "calibration/step2alum/runs/step2_best.rds", S2_START = "2005-01-01", S2_STOP = "2022-06-29")
source("calibration/step2/step2_common.R"); suppressMessages({library(dplyr); library(tidyr); library(ggplot2)})
src <- Sys.getenv("AL_FROM", "calibration/step2alum/runs"); frac <- as.numeric(Sys.getenv("AL_FRAC", "0.25"))
out <- file.path("calibration", "posterior"); fig <- file.path("website", "images", "alum_calib"); dir.create(out, showWarnings = FALSE, recursive = TRUE)
ies <- readRDS(file.path(src, "step2_ies.rds")); tab <- ies$tab; vary <- ies$vary; L <- ies$hist[[length(ies$hist)]]
to_val <- function(u, r) { if (identical(r$scale, "log") && r$min * r$max > 0) { lo <- log(min(abs(r$min), abs(r$max))); hi <- log(max(abs(r$min), abs(r$max))); sign(r$min) * exp(lo + u * (hi - lo)) } else r$min + u * (r$max - r$min) }
good <- which(is.finite(L$phi) & L$ok); good <- good[order(L$phi[good])][seq_len(ceiling(frac * length(good)))]
base <- readRDS("calibration/step2o/runs/step2_best.rds")$best; ak <- grepl("^aed_alum/", base$name)
members <- lapply(seq_along(good), function(m) { p <- base; for (j in seq_along(vary)) p$value[match(tab$name[vary[j]], p$name)] <- to_val(L$U[good[m], j], tab[vary[j], ]); p })
vals <- do.call(rbind, lapply(seq_along(members), function(m) { p <- members[[m]][ak, ]; data.frame(member = m, phi_alum_stage = L$phi[good[m]], setNames(as.list(p$value), sub("aed_alum/", "", p$name))) }))
saveRDS(members, file.path(out, "alum_members.rds")); write.csv(vals, file.path(out, "alum_members.csv"), row.names = FALSE); cat(length(members), "members\n"); print(round(vals, 4), row.names = FALSE)

run_set <- function(variant) {
  SA_VARIANT <<- variant; b <- step2_build(variant); a0 <- tryCatch(AEME::set_glm_balance_groups(b$aeme, groups = c("TN", "TP"), path = b$path), error = function(e) b$aeme)
  lapply(seq_along(members), function(m) { cat(variant, "member", m, "\n"); flush.console()
    res <- aemetools::run_aeme_param(b$aeme, param = members[[m]][, PCOLS], model = MODEL, path = b$path, return_aeme = TRUE); if (!inherits(res, "Aeme")) return(NULL)
    dv <- function(v) { d <- AEME::get_var(res, model = MODEL, var_sim = v, use_obs = FALSE, remove_spin_up = TRUE); d$Date <- as.Date(d$Date)
      rbind(d |> filter(depth <= 5) |> group_by(Date) |> summarise(value = mean(value), .groups = "drop") |> mutate(layer = "surface"), d |> filter(depth >= 17) |> group_by(Date) |> summarise(value = mean(value), .groups = "drop") |> mutate(layer = "bottom")) |> mutate(var = v) }
    bind_rows(lapply(c("PHS_tp", "PHS_frp", "PHY_tchla", "CHM_oxy"), dv)) |> mutate(member = m, variant = variant) }) |> bind_rows() }
S <- bind_rows(run_set("alum"), run_set("noalum")); saveRDS(S, file.path(out, "alum_ensemble_series.rds"))

S$yr <- as.integer(format(S$Date, "%Y")); S$wy <- S$yr + as.integer(as.integer(format(S$Date, "%m")) >= 7)
ann <- S |> filter(var != "CHM_oxy") |> group_by(variant, member, var, layer, yr) |> summarise(value = mean(value), .groups = "drop")
env <- ann |> group_by(variant, var, layer, yr) |> summarise(mean = mean(value), lo = min(value), hi = max(value), .groups = "drop")
anox <- S |> filter(var == "CHM_oxy", layer == "bottom") |> group_by(variant, member, wy) |> summarise(days = sum(value < 2), .groups = "drop")
cat("\nEnsemble spread of the 2007-2022 mean, alum ON vs OFF (min - mean - max over members):\n")
sm <- ann |> filter(yr >= 2007) |> group_by(variant, member, var, layer) |> summarise(value = mean(value), .groups = "drop") |> group_by(variant, var, layer) |> summarise(min = min(value), mean = mean(value), max = max(value), .groups = "drop")
print(sm |> mutate(across(where(is.numeric), \(x) signif(x, 3))) |> as.data.frame(), row.names = FALSE)
eff <- ann |> filter(yr >= 2007) |> group_by(variant, member, var, layer) |> summarise(value = mean(value), .groups = "drop") |> pivot_wider(names_from = variant, values_from = value) |> mutate(change_pct = 100 * (alum - noalum) / noalum) |> group_by(var, layer) |>
  summarise(min = min(change_pct), mean = mean(change_pct), max = max(change_pct), .groups = "drop"); cat("\nAlum effect (% change in the 2007-2022 mean, alum ON vs OFF):\n"); print(eff |> mutate(across(where(is.numeric), \(x) round(x, 1))) |> as.data.frame(), row.names = FALSE)
cat("\nAnoxic days per water year (bottom >= 17 m, all simulated days), alum ON: min/mean/max over members\n"); print(anox |> filter(variant == "alum", wy >= 2008) |> group_by(wy) |> summarise(min = min(days), mean = round(mean(days), 1), max = max(days)) |> as.data.frame(), row.names = FALSE)
write.csv(env, file.path(out, "alum_ensemble_annual_envelope.csv"), row.names = FALSE)
ggsave(file.path(fig, "alum_ensemble_envelope.png"), ggplot(env |> filter(yr >= 2005, layer == "surface"), aes(yr, mean, colour = variant, fill = variant)) + geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.25, colour = NA) + geom_line() +
         facet_wrap(~var, scales = "free_y") + labs(x = NULL, y = "Annual surface mean (0-5 m): min-max over alum-parameter members", colour = NULL, fill = NULL) + theme_minimal(base_size = 9), width = 9, height = 5, dpi = 120, bg = "white")

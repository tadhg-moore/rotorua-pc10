# Growth-limitation factors (light / N / P / Si) by group and month for the 20-year forward runs.
#   Rscript calibration/step2/run_limitation.R     (FWD_STAGES="b,e")
suppressMessages({library(dplyr); library(ggplot2)}); source("R/phyto_limitation_helpers.R")
stages <- strsplit(Sys.getenv("FWD_STAGES", "b,e"), ",")[[1]]; odir <- file.path("website", "images", "step2c")
res <- setNames(lapply(stages, function(s) phyto_limitation(file.path("calibration", "step2", "forward", s, "build", "fwd", "LID11133_rotorua", "glm_aed", "output", "output.nc"),
                                                            from = "1991-01-01", to = "2009-12-31")), stages)
for (s in stages) { cat("\n=====", s, ": mean over months, biomass-weighted, upper 5 m =====\n"); print(res[[s]]$overall |> mutate(across(where(is.numeric), \(x) round(x, 2))), row.names = FALSE)
  cat("\nsummer (Dec-Feb) / winter (Jun-Aug):\n"); sm <- res[[s]]$summary |> mutate(season = case_when(month %in% c(12, 1, 2) ~ "summer", month %in% 6:8 ~ "winter", TRUE ~ NA_character_)) |> filter(!is.na(season)) |>
    group_by(group, season) |> summarise(across(c(fT, fI, fN, fP, fSi, starts_with("limited_by")), mean), .groups = "drop"); print(sm |> mutate(across(where(is.numeric), \(x) round(x, 2))), row.names = FALSE)
  write.csv(res[[s]]$summary, file.path(odir, sprintf("limitation_%s.csv", s)), row.names = FALSE)
  ggsave(file.path(odir, sprintf("limitation_%s.png", s)), plot_phyto_limitation(res[[s]]), width = 7, height = 8, dpi = 120, bg = "white") }
saveRDS(res, file.path(odir, "limitation.rds"))

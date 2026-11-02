# One year with hourly output (nsave = 1): growth-limitation factors by hour of day, and by month for daylight hours only.
#   Rscript calibration/step2/run_limitation_hourly.R     (HL_STAGE = e, HL_START / HL_STOP, default 2002-07-01 / 2003-06-30)
stage <- Sys.getenv("HL_STAGE", "e")
Sys.setenv(S2_STAGE = stage, S2_DIR = file.path("calibration", "step2", "hourly_test"))
source("calibration/step2/step2_common.R"); source("R/phyto_limitation_helpers.R"); suppressMessages({library(dplyr); library(ggplot2)})
SA_START <- Sys.getenv("HL_START", "2002-07-01"); SA_STOP <- Sys.getenv("HL_STOP", "2003-06-30")
out <- file.path("calibration", "step2", "hourly_test"); odir <- file.path("website", "images", "step2c")
best <- readRDS(file.path(switch(stage, b = "calibration/step2b/runs", e = "calibration/step2e/runs", "calibration/step2/runs"), "step2_best.rds"))$best
b <- step2_build("hr")
cfg <- AEME::configuration(b$aeme); cfg[["glm_aed"]][["hydrodynamic"]][["output"]][["nsave"]] <- 1
AEME::configuration(b$aeme) <- cfg; AEME::write_configuration(aeme = b$aeme, model = MODEL, path = b$path, include_boundary = FALSE)
cat(grep("nsave|dt *=", readLines(list.files(b$path, "^glm4.nml$", recursive = TRUE, full.names = TRUE)[1]), value = TRUE), sep = "\n")
res <- aemetools::run_aeme_param(b$aeme, param = best[, PCOLS], model = MODEL, path = b$path, return_aeme = TRUE); stopifnot(inherits(res, "Aeme"))
nc <- file.path(AEME::get_lake_dir(res, b$path), "glm_aed", "output", "output.nc")
from <- as.Date(SA_START); to <- as.Date(SA_STOP)
all_h  <- phyto_limitation(nc, from, to)                                   # every output hour (instantaneous, includes night)
day_h  <- phyto_limitation(nc, from, to, par_min = 1)                      # daylight only
hourly <- phyto_limitation(nc, from, to, by = "hour")
saveRDS(list(all = all_h, day = day_h, hourly = hourly), file.path(odir, "limitation_hourly.rds"))
r <- function(d) d |> mutate(across(where(is.numeric), \(x) round(x, 2)))
cat("\nALL hours (instantaneous output, night included), mean of months:\n"); print(r(all_h$overall), row.names = FALSE)
cat("\nDAYLIGHT only (PAR > 1), mean of months:\n"); print(r(day_h$overall), row.names = FALSE)
cat("\nSummer (Dec-Feb), daylight only:\n"); print(r(day_h$summary |> filter(month %in% c(12, 1, 2)) |> group_by(group) |> summarise(across(c(fT, fI, fN, fP, fSi, starts_with("limited_by")), mean), .groups = "drop")), row.names = FALSE)
cat("\nBy hour of day (all months), cyano and green: light factor and share limited by light / P / N:\n")
print(r(hourly$summary |> filter(group %in% c("cyano", "green"), month %in% seq(0, 23, 3)) |> select(group, hour = month, fI, fN, fP, limited_by_light, limited_by_N, limited_by_P)), row.names = FALSE)
ggsave(file.path(odir, "limitation_hourly_hour.png"), ggplot(hourly$summary |> tidyr::pivot_longer(c(fI, fN, fP, fSi), names_to = "factor", values_to = "value"), aes(month, value, colour = factor)) +
         geom_line() + facet_wrap(~group, ncol = 1) + labs(x = "Hour of day", y = "Mean limitation factor (1 = none)", colour = NULL) + theme_minimal(base_size = 9), width = 7, height = 7, dpi = 120, bg = "white")
ggsave(file.path(odir, "limitation_hourly_month_day.png"), plot_phyto_limitation(day_h), width = 7, height = 8, dpi = 120, bg = "white")

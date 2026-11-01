suppressMessages({library(AEME); library(dplyr)})
base <- readRDS("calibration/step3/aeme_base_2005_alum.rds")
inf <- AEME::inflows(base)$data; cat("class", class(inf), " names:", paste(names(inf), collapse = ", "), "\n")
x <- inf[[1]]; cat("class of element:", class(x), "
"); cat("inner:", paste(names(x), collapse = ", "), "\n")
MW <- 26.981539
tab <- bind_rows(lapply(c("Utuhina", "Puarenga"), function(n) { d <- inf[[n]]; d$Date <- as.Date(d$Date)
  # flow is in m3/d in the AEME inflow tables (run_job divides by 86400)
  d$al_t <- d$ALU_ala * d$HYD_flow / 1e6 * MW / 1000   # tonnes Al per day, assuming ratio_Al_P = 1 (mmol P capacity = mmol Al)
  d$yr <- as.integer(format(d$Date, "%Y")); d |> group_by(yr) |> summarise(al_t = sum(al_t, na.rm = TRUE), days_dosed = sum(ALU_ala > 0, na.rm = TRUE), mean_ala = mean(ALU_ala[ALU_ala > 0]), .groups = "drop") |> mutate(stream = n) }))
print(tidyr::pivot_wider(tab |> filter(yr >= 2005) |> mutate(across(c(al_t, mean_ala), \(v) round(v, 1))), names_from = stream, values_from = c(al_t, days_dosed, mean_ala)), n = 30)
cat("\ntotal Al dosed in model (t), 2005-2022: Utuhina", round(sum(tab$al_t[tab$stream == "Utuhina"])), " Puarenga", round(sum(tab$al_t[tab$stream == "Puarenga"])), " both", round(sum(tab$al_t)), " (report: 993 t to end 2023)\n")

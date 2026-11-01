suppressMessages({library(AEME); library(dplyr)})
inf <- AEME::inflows(readRDS("calibration/step3/aeme_base_2005_alum.rds"))$data
for (n in c("Utuhina", "Puarenga")) { d <- inf[[n]]; d$Date <- as.Date(d$Date); d <- d[d$Date >= "2011-01-01" & d$Date <= "2017-12-31" & d$ALU_ala > 0, ]
  cat(n, ": flow m3/s median", round(median(d$HYD_flow) / 86400, 2), "| ALU_ala mmol P/m3 median", round(median(d$ALU_ala), 1), "| inflow FRP mmol/m3 median", round(median(d$PHS_frp), 2),
      "| TP-ish (frp+dop+pop+pip) median", round(median(d$PHS_frp + d$PHS_dop + d$PHS_pop + d$PHS_pip), 2), "| capacity : FRP =", round(median(d$ALU_ala / d$PHS_frp), 1), "\n") }

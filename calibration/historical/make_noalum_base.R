# No-alum counterpart of the flood x0.5 base: removes the alum dose (ALU_ala) and the stream-bound pool (ALU_alp) and returns the bound P to the inflow FRP,
# so the inflows are the untreated series (bound P B in mmol/m3 -> FRP + B / MMOL, MMOL = 1000/30.974 as in calibration/alum/stream_variant.R).
suppressPackageStartupMessages(library(AEME))
MMOL <- 1000 / 30.974
base <- readRDS("calibration/alum/stream/base_Q50.rds"); inf <- AEME::inflows(base)$data
for (s in names(inf)) { d <- inf[[s]]; if (!"ALU_alp" %in% names(d)) next
  b <- d$ALU_alp; b[is.na(b)] <- 0; cat(s, ": mean bound P returned (mmol/m3)", signif(mean(b), 3), "| dose days", sum(d$ALU_ala > 0, na.rm = TRUE), "\n")
  d$PHS_frp <- d$PHS_frp + b / MMOL; d$ALU_ala <- 0; d$ALU_alp <- 0; inf[[s]] <- d }
base <- base |> AEME::remove_inflow(all = TRUE) |> AEME::add_inflows(inf)
saveRDS(base, "calibration/alum/stream/base_Q50_noalum.rds"); cat("saved\n")

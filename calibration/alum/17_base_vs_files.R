suppressMessages({library(AEME); library(dplyr); library(readr)})
inf <- AEME::inflows(readRDS("calibration/step3/aeme_base_2005_alum.rds"))$data
w <- read_csv("data/raw/flows/rotorua_inflows_populated_withalum.csv", show_col_types = FALSE); n <- read_csv("data/raw/flows/rotorua_inflows_populated_noalum.csv", show_col_types = FALSE)
mp <- c(HYD_flow = "VOL", HYD_temp = "TEMPTURE", NIT_amm = "NH4", NIT_nit = "NO3", NIT_don = "DONL", NIT_pon = "PONL", PHS_frp = "DRP", PHS_dop = "DOPL", PHS_pop = "POPL", PHS_pip = "PIP", CAR_doc = "DOCL", CAR_poc = "POCL", NCS_ss1 = "SSOL1", NCS_ss2 = "SSOL2")
for (s in c("Utuhina", "Puarenga", "Hamurana", "Ungauged-Quick")) {
  b <- inf[[s]]; b$Date <- as.Date(b$Date); b <- b[b$Date >= "2005-01-01" & b$Date <= "2022-06-29", ]
  res <- sapply(names(mp), \(v) { cmp <- \(f) { f <- f[f$Inflow == s, ]; f$Date <- as.Date(f$Date); m <- inner_join(b[, c("Date", v)], f[, c("Date", mp[[v]])], by = "Date"); mean(abs(m[[2]] - m[[3]]) > 1e-6 * pmax(1, abs(m[[2]])), na.rm = TRUE) }; c(with = cmp(w), no = cmp(n)) })
  cat("\n", s, "- fraction of days where base differs from the file (with | no):\n"); print(round(res, 2))
}

# Lake stoichiometry set in R, without a new parameter in aed_alum.F90.
# The module uses capacity one for one with P (ALU_ala binds FRP mol per mol). The thesis jar-test ratio for Lake Rotorua is 0.7 mol Al per mol P, so one mol of Al binds beta = 1 / 0.7 = 1.43 mol P.
# Treat ALU_ala and ALU_alp as P-binding capacity, in mmol P/m3: the dose delivered to the module is beta x the Al dose, and the stream step binds B = min(f_max * FRP, eta * capacity) out of it.
# Aluminium is capacity / beta wherever it is reported (Al:P ratios, ERI 178 aluminium).
# Not independent of the lake rates: capacity is lost mostly by ageing and settling (57% and 28% of the water column losses in member 46), so a larger beta raises P uptake by much less than beta, and it trades off with k_w and k_age_w. Fit the rates with beta fixed.
#   Rscript calibration/alum/63_stoich_base.R [beta]   (default 1/0.7)   -> calibration/alum/stream/base_H1b_B<beta*100>.rds
suppressMessages({library(AEME); library(dplyr)})
MMOL <- 1000 / 30.974; FMAX <- 0.9
args <- commandArgs(TRUE); beta <- if (length(args)) as.numeric(args[1]) else 1 / 0.7; tag <- sprintf("H1b_B%03d", round(beta * 100))
b0 <- readRDS("calibration/alum/stream/base_Q50.rds"); inf <- AEME::inflows(b0)$data
# stream_base(): untreated FRP and the Al dose are recovered from the previous base (exact, see 60_h1_base.R), the dose becomes capacity (beta x Al), and the stream step is applied to it.
stream_base <- function(d, beta, rule, eta = 1) { frp0 <- d$PHS_frp * MMOL + d$ALU_alp; cap <- beta * (d$ALU_ala + d$ALU_alp); R <- ifelse(frp0 > 0, cap / frp0, 0)
  B <- pmin(rule(R) * frp0, eta * cap); B[cap <= 0 | frp0 <= 0] <- 0; d$PHS_frp <- (frp0 - B) / MMOL; d$ALU_ala <- cap - B; d$ALU_alp <- B; d }
rules <- list(Puarenga = \(R) rep(FMAX, length(R)), Utuhina = \(R) rep(0.45, length(R)))   # Puarenga: thesis stoichiometry (capacity limited, cap 0.9); Utuhina: the measured removal (see 62_stream_fit.R)
for (s in names(rules)) inf[[s]] <- stream_base(inf[[s]], beta, rules[[s]])
b <- b0 |> AEME::remove_inflow(all = TRUE) |> AEME::add_inflows(inf); f <- file.path("calibration/alum/stream", paste0("base_", tag, ".rds")); saveRDS(b, f)
for (s in names(rules)) { d <- inf[[s]]; d <- d[d$Date >= "2006-01-01" & d$Date <= "2022-06-29" & d$ALU_ala + d$ALU_alp > 0, ]; cat(s, ": capacity delivered", round(sum((d$ALU_ala + d$ALU_alp) * d$HYD_flow) * 26.98 / 1e9), "t Al-equivalent (", round(sum((d$ALU_ala + d$ALU_alp) * d$HYD_flow) * 26.98 / 1e9 / beta), "t Al ), P bound in the stream", round(sum(d$ALU_alp * d$HYD_flow) * 30.974 / 1e9, 1), "t\n") }
cat("beta =", round(beta, 3), " saved", f, "\n")

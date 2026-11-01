# Hypothesis H1 for the in-stream alum step: the thesis stoichiometry (Sanila MSc): where DRP stays measurable the Al:P molar ratio is near 1:1 (Lake Rotorua 0.7:1, Puarenga 0.98:1).
# The stream step then binds B = min(f_max * FRP, eps * dose) with eps = 1 (mol P per mol Al dosed), instead of the McIntosh ramp (Puarenga 0.9 * min(1, R/23)) and the flat 0.45 (Utuhina).
#   H1  : both dosed streams use the thesis rule
#   H1b : Puarenga uses the thesis rule, Utuhina keeps the measured 0.45 (the Utuhina inflow P was measured downstream of the dosing)
# Starts from base_Q50 (V1 rule already applied): the untreated FRP and the dose are rebuilt from it, so the flood x0.5 particulate P and every other inflow variable are unchanged.
#   Rscript calibration/alum/60_h1_base.R      -> calibration/alum/stream/base_H1.rds, base_H1b.rds, website/data/h1/stream_rules.csv
suppressMessages({library(AEME); library(dplyr); library(readr)})
MMOL <- 1000 / 30.974; FMAX <- 0.9; EPS <- 1
b0 <- readRDS("calibration/alum/stream/base_Q50.rds"); inf0 <- AEME::inflows(b0)$data; dir.create("website/data/h1", recursive = TRUE, showWarnings = FALSE)
rule_old <- list(Utuhina = function(R) rep(0.45, length(R)), Puarenga = function(R) 0.9 * pmin(1, R / 23))
untreated <- function(d) { frp0 <- d$PHS_frp * MMOL + d$ALU_alp; list(frp0 = frp0, dose = d$ALU_ala + d$ALU_alp) }   # mmol/m3
apply_rule <- function(d, how) { u <- untreated(d); R <- ifelse(u$frp0 > 0, u$dose / u$frp0, 0)
  B <- switch(how, old_P = , old_U = pmin(rule_old[[if (how == "old_P") "Puarenga" else "Utuhina"]](R) * u$frp0, u$dose), thesis = pmin(FMAX * u$frp0, EPS * u$dose)); B[u$dose <= 0 | u$frp0 <= 0] <- 0
  d$PHS_frp <- (u$frp0 - B) / MMOL; d$ALU_ala <- u$dose - B; d$ALU_alp <- B; d }
# check: the old rule applied to the untreated series must give back base_Q50 (so the rebuild is exact)
for (s in c("Utuhina", "Puarenga")) { d <- inf0[[s]]; e <- apply_rule(d, if (s == "Puarenga") "old_P" else "old_U"); cat(s, "rebuild check, max |diff| ALU_alp:", signif(max(abs(e$ALU_alp - d$ALU_alp)), 3), " PHS_frp:", signif(max(abs(e$PHS_frp - d$PHS_frp)), 3), "\n") }
mk <- function(name, how) { inf <- inf0; for (s in names(how)) inf[[s]] <- apply_rule(inf0[[s]], how[[s]])
  b <- b0 |> AEME::remove_inflow(all = TRUE) |> AEME::add_inflows(inf); saveRDS(b, file.path("calibration/alum/stream", paste0("base_", name, ".rds"))); inf }
sm <- function(inf, name) bind_rows(lapply(c("Utuhina", "Puarenga"), function(s) { d <- inf[[s]]; d <- d[d$Date >= "2006-01-01" & d$Date <= "2022-06-29" & d$ALU_ala + d$ALU_alp > 0, ]; u <- untreated(d); B <- d$ALU_alp; vol <- d$HYD_flow
  f <- B / u$frp0; sat <- f >= FMAX - 1e-6; tt <- function(x) sum(x * vol) / 1e6 / 1e3   # t of the element: mmol/m3 * m3/d -> mol -> t via the molar mass below
  data.frame(run = name, stream = s, dosed_days = nrow(d), Al_t = sum(u$dose * vol) * 26.98 / 1e9, P_bound_t = sum(B * vol) * 30.974 / 1e9, ratio_molmol = sum(u$dose * vol) / sum(B * vol), frac_FRP_removed = sum(B * vol) / sum(u$frp0 * vol),
    days_at_cap = mean(sat), Al_share_on_capped_days = sum((u$dose * vol)[sat]) / sum(u$dose * vol), ratio_molmol_uncapped = sum((u$dose * vol)[!sat]) / sum((B * vol)[!sat])) }))
H1 <- mk("H1", c(Utuhina = "thesis", Puarenga = "thesis")); H1b <- mk("H1b", c(Puarenga = "thesis"))
out <- bind_rows(sm(inf0, "Q50 (previous)"), sm(H1, "H1"), sm(H1b, "H1b")); print(as.data.frame(out |> mutate(across(where(is.numeric), \(x) signif(x, 3)))), row.names = FALSE)
write_csv(out |> mutate(across(where(is.numeric), \(x) signif(x, 4))), "website/data/h1/stream_rules.csv"); cat("saved base_H1.rds and base_H1b.rds\n")

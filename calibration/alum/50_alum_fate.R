# Alum applied to P removed, and the fate of alum and P, from the best member (member 46 of stage j, new AEME dev and GLM-AED, run kept by run_hist.R with HIST_KEEP=1).
#   Rscript calibration/alum/50_alum_fate.R
# Streams: the dose capacity of the inflow is ALU_ala + ALU_alp (mmol Al per m3, capacity ratio r = 1); the bound P of the stream step is ALU_alp. The observed removal is the untreated minus treated inflow series (data/raw/flows).
# Lake: pelagic and benthic uptake come from the module diagnostics (ALU_uptake_w, ALU_uptake_s). Ageing, burial and resuspension are k x pool (not model output), settling is the closure of the benthic balance, and the pelagic balance is the check.
# Outputs: website/data/alumfate/*.csv, website/images/alumfate/*.png
suppressMessages({library(dplyr); library(tidyr); library(readr); library(ggplot2); library(AEME)})
d_out <- "website/data/alumfate"; i_out <- "website/images/alumfate"; for (d in c(d_out, i_out)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
MP <- 30.974; MAL <- 26.98; START <- as.Date("2006-01-01"); END <- as.Date("2022-06-29")
run_dir <- "calibration/historical/out_budget_m46"; a <- readRDS(file.path(run_dir, "aeme_46.rds"))
nc_file <- AEME::get_model_outfile(a)$glm_aed[["output"]]; stopifnot(file.exists(nc_file))
par_nml <- readLines(file.path(dirname(dirname(nc_file)), "aed", "aed.nml")); alum_nml <- par_nml[(grep("^&aed_alum", par_nml)):length(par_nml)]; alum_nml <- alum_nml[seq_len(grep("^/", alum_nml)[1])]
gp <- function(n) as.numeric(sub(".*=\\s*([-0-9.eE+]+).*", "\\1", alum_nml[grep(paste0("^\\s*", n, "\\s*="), alum_nml)[1]]))
K <- c(k_w = gp("k_w"), k_s = gp("k_s"), K_P = gp("K_P"), w_ala = gp("w_ala"), k_age_w = gp("k_age_w"), k_age_s = gp("k_age_s"), k_bur = gp("k_bur"), k_res = gp("k_res")); print(K)

# ---- 1. streams: dose, P bound in the stream step, observed removal ------------------------------------------------------------------------------------------
inf <- AEME::inflows(a)$data
S <- bind_rows(lapply(c("Utuhina", "Puarenga"), function(s) { d <- inf[[s]]; d$Date <- as.Date(d$Date)
  data.frame(stream = s, Date = d$Date, Q = d$HYD_flow, ala = d$ALU_ala, alp = d$ALU_alp, frp = d$PHS_frp) })) |> filter(Date >= START, Date <= END) |> mutate(dose = ala + alp, year = as.integer(format(Date, "%Y")))
rd <- function(f) read_csv(file.path("data/raw/flows", f), show_col_types = FALSE) |> mutate(Date = as.Date(Date)) |> filter(Inflow %in% c("Utuhina", "Puarenga"), Date >= START, Date <= END) |> select(stream = Inflow, Date, VOL, DRP, TP)
O <- inner_join(rd("rotorua_inflows_populated_noalum.csv"), rd("rotorua_inflows_populated_withalum.csv"), by = c("stream", "Date"), suffix = c("_no", "_with"))
S <- S |> left_join(O |> select(stream, Date, DRP_no, DRP_with, TP_no, TP_with), by = c("stream", "Date")) |>
  mutate(Al_mmol = dose * Q, B_mmol = alp * Q, drp_rem_mmol = pmax(DRP_no - DRP_with, 0) * Q * 1000 / MP, frp_in_mmol = DRP_no * Q * 1000 / MP)
st <- function(x) x |> summarise(days = sum(dose > 0), Al_t = sum(Al_mmol) * MAL / 1e9, frp_in_t = sum(frp_in_mmol) * MP / 1e9, B_t = sum(B_mmol) * MP / 1e9, obs_drp_rem_t = sum(drp_rem_mmol) * MP / 1e9,
  ratio_model = sum(Al_mmol) / sum(B_mmol), ratio_obs_drp = sum(Al_mmol) / sum(drp_rem_mmol), f_model = sum(B_mmol) / sum(frp_in_mmol), f_obs = sum(drp_rem_mmol) / sum(frp_in_mmol))
dosed <- S |> filter(dose > 0)
str_all <- dosed |> group_by(stream) |> st(); str_year <- dosed |> group_by(stream, year) |> st() |> mutate(Al_t_yr = Al_t, B_t_yr = B_t)
# by dosage ratio R = dose / FRP (mol Al per mol DRP)
rb <- c(0, 2, 5, 10, 23, 50, Inf); S$R <- ifelse(S$frp_in_mmol > 0, S$Al_mmol / S$frp_in_mmol, NA); dosed <- S |> filter(dose > 0, is.finite(R)); dosed$Rbin <- cut(dosed$R, rb, include.lowest = TRUE)
str_R <- dosed |> group_by(stream, Rbin) |> st()
write_csv(str_all |> mutate(across(where(is.numeric), \(z) signif(z, 4))), file.path(d_out, "stream_totals.csv")); write_csv(str_year |> mutate(across(where(is.numeric), \(z) signif(z, 4))), file.path(d_out, "stream_by_year.csv")); write_csv(str_R |> mutate(across(where(is.numeric), \(z) signif(z, 4))), file.path(d_out, "stream_by_R.csv"))
cat("\nStreams (dosed days, 2006-2022):\n"); print(as.data.frame(str_all |> mutate(across(where(is.numeric), \(z) signif(z, 3)))))
cat("\nBy dosage ratio R:\n"); print(as.data.frame(str_R |> mutate(across(where(is.numeric), \(z) signif(z, 3)))))

# ---- 2. lake: pools and fluxes from the netcdf ------------------------------------------------------------------------------------------------------------
nc <- ncdf4::nc_open(nc_file); on.exit(ncdf4::nc_close(nc))
tm <- ncdf4::ncvar_get(nc, "time"); orig <- as.Date(strsplit(ncdf4::ncatt_get(nc, "time", "units")$value, " ")[[1]][3]); dd <- orig + floor(tm / 24)
ns <- ncdf4::ncvar_get(nc, "NS"); nl <- dim(ncdf4::ncvar_get(nc, "V", start = c(1, 1, 1, 1), count = c(1, 1, -1, 1)))[1]
gm <- function(v) { x <- ncdf4::ncvar_get(nc, v); x[!is.finite(x) | abs(x) > 1e20] <- 0; x }
Vl <- gm("V"); ala <- gm("ALU_ala"); alp <- gm("ALU_alp"); upw <- gm("ALU_uptake_w"); valid <- row(Vl) <= matrix(ns, nrow(Vl), length(ns), byrow = TRUE); Vl[!valid] <- 0
Wa <- colSums(ala * Vl); Wp <- colSums(alp * Vl); Uw <- colSums(upw * Vl)
surf <- cbind(ala = ala[cbind(ns, seq_along(ns))], alp = alp[cbind(ns, seq_along(ns))]); qout <- {o <- AEME::aeme_to_inflow(a); o$HYD_flow[match(dd, as.Date(o$Date))]}; qout[!is.finite(qout)] <- 0
za <- ncdf4::ncvar_get(nc, "zarea")[1:4, ]; za[!is.finite(za)] <- 0
bz <- function(v) { x <- ncdf4::ncvar_get(nc, v)[1:4, ]; x[!is.finite(x)] <- 0; colSums(x * za) }
Ba <- bz("ALU_ben_ala_Z"); Bp <- bz("ALU_ben_alp_Z"); Us <- bz("ALU_uptake_s_Z")
L <- data.frame(Date = dd, Wa, Wp, Ba, Bp, Uw, Us, Out_a = surf[, "ala"] * qout, Out_p = surf[, "alp"] * qout, qout) |> filter(Date >= START, Date <= END)
inflow <- S |> group_by(Date) |> summarise(In_a = sum(ala * Q), In_p = sum(alp * Q), .groups = "drop")
# other inflows carry no alum; check
oth <- sapply(setdiff(names(inf), c("Utuhina", "Puarenga")), function(s) sum(abs(inf[[s]]$ALU_ala), abs(inf[[s]]$ALU_alp), na.rm = TRUE)); cat("alum in other inflows (sum):", sum(oth), "\n")
L <- L |> left_join(inflow, by = "Date") |> mutate(across(c(In_a, In_p), \(z) coalesce(z, 0)))
n <- nrow(L); dS <- function(x) c(diff(x), NA)
L <- L |> mutate(Age_w = K["k_age_w"] * Wa, Age_s = K["k_age_s"] * Ba, Bur_a = K["k_bur"] * Ba, Bur_p = K["k_bur"] * Bp, Res_a = K["k_res"] * Ba, Res_p = K["k_res"] * Bp,
  Set_a = dS(Ba) + Us + Age_s + Bur_a + Res_a, Set_p = dS(Bp) - Us + Bur_p + Res_p) |> filter(!is.na(Set_a))
tot <- function(x) sum(x)
mm <- function(x, M) x * M / 1e9                    # mmol -> t
yrs <- as.numeric(difftime(max(L$Date), min(L$Date), units = "days")) / 365.25
T_ <- list(
  `Capacity (Al, mmol)` = c(`Inflow capacity (ALU_ala)` = tot(L$In_a), `Used in pelagic uptake` = -tot(L$Uw), `Used in benthic uptake` = -tot(L$Us), `Lost by pelagic ageing` = -tot(L$Age_w), `Lost by benthic ageing` = -tot(L$Age_s), `Buried (capacity)` = -tot(L$Bur_a), `Out through the outlet` = -tot(L$Out_a), `Change in pelagic pool` = -(L$Wa[n - 1] - L$Wa[1]), `Change in benthic pool` = -(L$Ba[n - 1] - L$Ba[1])))
cat("\nclosure checks\n")
pel_a <- tot(L$In_a) + tot(L$Res_a) - tot(L$Set_a) - tot(L$Uw) - tot(L$Age_w) - tot(L$Out_a) - (L$Wa[n - 1] - L$Wa[1])
pel_p <- tot(L$In_p) + tot(L$Uw) + tot(L$Res_p) - tot(L$Set_p) - tot(L$Out_p) - (L$Wp[n - 1] - L$Wp[1])
cat("pelagic capacity balance residual (mmol):", signif(pel_a, 3), " as % of capacity input:", round(100 * pel_a / tot(L$In_a), 2), "\npelagic bound-P balance residual (mmol):", signif(pel_p, 3), " as % of bound P input:", round(100 * pel_p / (tot(L$In_p) + tot(L$Uw) + tot(L$Us)), 2), "\n")
# fate table, per year, t Al or t P
F <- data.frame(
  quantity = c("Al applied to the dosed streams", "Al used in the stream step (bound P)", "Al delivered to the lake as free capacity (ALU_ala)", "Al delivered to the lake as bound P (ALU_alp)",
               "P bound in the stream step", "P bound in the water column", "P bound in settled alum (benthic uptake)", "P bound in total by alum",
               "Capacity lost to ageing, water column", "Capacity lost to ageing, benthic", "Settling flux of capacity (ala)", "Settling flux of bound P (alp)",
               "Resuspension of bound P", "Resuspension of capacity", "Burial of bound P", "Burial of capacity (Al total)", "Bound P out through the outlet", "Capacity out through the outlet"),
  value_mmol = c(sum(S$Al_mmol), sum(S$B_mmol), tot(L$In_a), tot(L$In_p), tot(L$In_p), tot(L$Uw), tot(L$Us), tot(L$In_p) + tot(L$Uw) + tot(L$Us), tot(L$Age_w), tot(L$Age_s), tot(L$Set_a), tot(L$Set_p), tot(L$Res_p), tot(L$Res_a), tot(L$Bur_p), tot(L$Bur_a) + tot(L$Bur_p), tot(L$Out_p), tot(L$Out_a)),
  unit = c("Al", "Al", "Al", "Al", rep("P", 4), rep("Al", 2), "Al", "P", "P", "Al", "P", "Al", "P", "Al"))
F$t_total <- ifelse(F$unit == "Al", mm(F$value_mmol, MAL), mm(F$value_mmol, MP)); F$t_per_yr <- F$t_total / yrs
write_csv(F |> mutate(across(where(is.numeric), \(z) signif(z, 4))), file.path(d_out, "fate_totals.csv")); cat("\nFate totals (", round(yrs, 2), "years, t):\n"); print(as.data.frame(F |> mutate(across(where(is.numeric), \(z) signif(z, 3)))))
pools <- data.frame(pool = c("Water column capacity (ala)", "Water column bound P (alp)", "Settled capacity (ben_ala)", "Settled bound P (ben_alp)"), end_mmol = c(L$Wa[n - 1], L$Wp[n - 1], L$Ba[n - 1], L$Bp[n - 1]), max_mmol = c(max(L$Wa), max(L$Wp), max(L$Ba), max(L$Bp)), mean_mmol = c(mean(L$Wa), mean(L$Wp), mean(L$Ba), mean(L$Bp)))
pools$end_t <- pools$end_mmol * c(MAL, MP, MAL, MP) / 1e9; pools$mean_t <- pools$mean_mmol * c(MAL, MP, MAL, MP) / 1e9; write_csv(pools |> mutate(across(where(is.numeric), \(z) signif(z, 4))), file.path(d_out, "pools.csv")); print(as.data.frame(pools |> mutate(across(where(is.numeric), \(z) signif(z, 3)))))
L$year <- as.integer(format(L$Date, "%Y")); Y <- L |> group_by(year) |> summarise(across(c(In_a, In_p, Uw, Us, Set_a, Set_p, Res_a, Res_p, Bur_a, Bur_p, Out_a, Out_p, Age_w, Age_s), \(z) sum(z)), Wa_end = last(Wa), Wp_end = last(Wp), Ba_end = last(Ba), Bp_end = last(Bp), days = n(), .groups = "drop")
write_csv(Y |> mutate(across(where(is.numeric), \(z) signif(z, 4))), file.path(d_out, "lake_by_year_mmol.csv")); saveRDS(list(S = S, L = L, K = K, F = F, pools = pools, yrs = yrs, str_all = str_all, str_year = str_year, str_R = str_R), file.path(d_out, "alum_fate.rds"))
cat("done\n")

# ---- 3. extra summaries and figures ---------------------------------------------------------------------------------------------------------------------
# per-pass fractions of settled material (rates only, so they do not depend on the pool sizes)
kb <- K[["k_bur"]]; kr <- K[["k_res"]]; ka <- K[["k_age_s"]]
fr <- data.frame(pool = c("Settled bound P (ben_alp)", "Settled bound P (ben_alp)", "Settled capacity (ben_ala)", "Settled capacity (ben_ala)", "Settled capacity (ben_ala)"), route = c("Buried", "Resuspended", "Buried", "Aged", "Resuspended"),
  rate = c(kb, kr, kb, ka, kr)); fr <- fr |> group_by(pool) |> mutate(fraction = rate / sum(rate)) |> ungroup(); write_csv(fr |> mutate(across(where(is.numeric), \(z) signif(z, 4))), file.path(d_out, "settled_fractions.csv")); print(as.data.frame(fr))
# pelagic capacity: share lost to ageing vs settling vs uptake (flux-weighted)
pel <- data.frame(route = c("Aged out", "Settled", "Bound P (uptake)", "Out through the outlet"), mmol = c(tot(L$Age_w), tot(L$Set_a), tot(L$Uw), tot(L$Out_a))); pel$share <- pel$mmol / sum(pel$mmol); write_csv(pel |> mutate(across(where(is.numeric), \(z) signif(z, 4))), file.path(d_out, "pelagic_capacity_fate.csv")); print(pel)
# ratio summaries, mol Al : mol P
rr <- data.frame(basis = c("Streams, stream step (model rule)", "Streams, stream step (observed removal, DRP)", "Lake, free capacity delivered per P bound (uptake diagnostic)", "Whole system, Al applied per P bound (stream + lake, uptake diagnostic)"),
  ratio = c(sum(S$Al_mmol) / sum(S$B_mmol), sum(S$Al_mmol) / sum(S$drp_rem_mmol), tot(L$In_a) / (tot(L$Uw) + tot(L$Us)), sum(S$Al_mmol) / (tot(L$In_p) + tot(L$Uw) + tot(L$Us)))); print(rr); write_csv(rr |> mutate(ratio = signif(ratio, 4)), file.path(d_out, "ratios.csv"))
bud <- read_csv("website/data/budget/budget_closure_pair.csv", show_col_types = FALSE) |> filter(element == "P")
imp <- (bud$`No alum`[bud$term == "outflow"] - bud$`With observed alum`[bud$term == "outflow"]) / 1000; res_d <- (bud$`With observed alum`[bud$term == "residual"] - bud$`No alum`[bud$term == "residual"]) / 1000
alP_t <- (sum(S$B_mmol) + tot(L$Uw) + tot(L$Us)) * MP / 1e9 / yrs; Al_t <- sum(S$Al_mmol) * MAL / 1e9 / yrs
chk <- data.frame(quantity = c("Al applied (t Al/yr)", "P bound by alum, uptake diagnostics (t P/yr)", "Reduction in P export, alum vs no alum (t P/yr)", "Difference in unexplained P between runs (t P/yr)", "Al:P, applied per P bound (diagnostics)", "Al:P, applied per P export reduction", "Al:P, applied per unexplained-P difference"),
  value = c(Al_t, alP_t, imp, -res_d, (Al_t / MAL) / (alP_t / MP), (Al_t / MAL) / (imp / MP), (Al_t / MAL) / (-res_d / MP))); print(chk); write_csv(chk |> mutate(value = signif(value, 4)), file.path(d_out, "budget_crosscheck.csv"))
th <- theme_minimal(base_size = 9) + theme(legend.position = "bottom")
sr <- str_R |> mutate(R_mid = c(1, 3.5, 7.5, 16.5, 36.5, 75)[as.integer(Rbin)]) |> pivot_longer(c(ratio_model, ratio_obs_drp), names_to = "basis", values_to = "ratio") |> mutate(basis = c(ratio_model = "Stream rule", ratio_obs_drp = "Observed removal (file)")[basis])
p1 <- ggplot(sr, aes(R_mid, ratio, colour = stream, linetype = basis, shape = basis)) + geom_hline(yintercept = c(0.7, 7), colour = "grey55", linetype = "dotted") + annotate("text", x = 1, y = c(0.9, 9), label = c("jar test 0.7", "field 7"), size = 2.6, hjust = 0, colour = "grey35") + geom_line() + geom_point(size = 2) +
  scale_x_log10() + scale_y_log10() + scale_colour_manual(values = c(Puarenga = "#1b9e77", Utuhina = "#7570b3")) + labs(x = "Dose ratio of the day, mol Al per mol inflow DRP (bin centre, log)", y = "Al applied per P removed in the stream (mol/mol, log)", colour = NULL, linetype = NULL, shape = NULL) + th
ggsave(file.path(i_out, "stream_ratio_by_R.png"), p1, width = 7, height = 4.4, dpi = 130, bg = "white")
yy <- str_year |> filter(year <= 2021, year >= 2007) |> select(stream, year, Al_t, B_t, obs_drp_rem_t) |> pivot_longer(c(Al_t, B_t), names_to = "k", values_to = "t") |> mutate(k = c(Al_t = "Al applied (t Al/yr)", B_t = "P bound in the stream step (t P/yr)")[k])
ggsave(file.path(i_out, "stream_annual.png"), ggplot(yy, aes(year, t, fill = stream)) + geom_col(position = position_dodge(0.8), width = 0.75) + facet_wrap(~k, scales = "free_y", ncol = 1) + scale_fill_manual(values = c(Puarenga = "#1b9e77", Utuhina = "#7570b3")) + labs(x = NULL, y = NULL, fill = NULL) + th, width = 7, height = 5, dpi = 130, bg = "white")
cat("done 3\n")

# Build a bloom-seeded prior ensemble for a lake's stage-4 calibration from its bloom screen
# (sa_bloom.R). Keeps the forward-model runs whose modelled surface chl-a (and, where the lake has a
# meaningful cyano signal, cyano) fall in the observed range and writes their parameter sets as a CSV
# (rows = accepted runs, columns = aemetools name_full) for  PEST_BLOOM_PRIOR=<csv>  in pest_calib_v6.R.
#
#   Rscript R/pest_workflow/build_bloom_prior.R <LAKE_TAG>      (env PEST_WF_ROOT as for sa_bloom.R)
#   env: BP_FACTOR_MEAN (default 2.5), BP_FACTOR_P90 (3), BP_MIN_RUNS (40)
#
# Acceptance (modelled vs observed surface <= 2 m, simulation window):
#   mean chl-a within [obs_mean / FM, obs_mean * FM]   and   p90 within [obs_p90 / FP, obs_p90 * FP]
#   cyano (only if obs cyano mean > 5% of obs chl-a mean and >= 10 obs): p90 within a factor 5 of the obs p90
# If fewer than BP_MIN_RUNS pass, the factors are widened by 1.4x up to twice; if still too few nothing is
# written (the stage then runs unseeded) and the exit status is 2.
suppressMessages({ library(targets); library(AEME); library(dplyr) })
source("R/pest_workflow/manifest.R")

tag <- { a <- commandArgs(trailingOnly = TRUE); if (length(a)) a[1] else stop("usage: build_bloom_prior.R <LAKE_TAG>") }
FM <- as.numeric(Sys.getenv("BP_FACTOR_MEAN", "2.5")); FP <- as.numeric(Sys.getenv("BP_FACTOR_P90", "3"))
MIN_RUNS <- as.integer(Sys.getenv("BP_MIN_RUNS", "40"))
d <- file.path(PEST_WF_ROOT, tag, "diagnostics", "sa_bloom")
runs <- read.csv(file.path(d, "runs.csv")); par <- read.csv(file.path(d, "params.csv"), check.names = FALSE)
stopifnot(all(runs$run %in% par$run))

# ---- observed targets -------------------------------------------------------------------------------
lid <- sub("_.*", "", tag)
al <- c(tar_read(aeme_baseline_upstream), tar_read(aeme_baseline_downstream))
a <- al[[which(vapply(al, function(x) identical(lake(x)$id, lid), NA))[1]]]
st <- tar_read(simulation_time)
o <- observations(a)$lake; o$Date <- as.Date(o$Date)
o <- o[which(o$Date >= as.Date(st$start) & o$Date <= as.Date(st$stop) & o$depth <= 2 & is.finite(o$value)), ]
ob <- function(v) o$value[which(o$var_aeme == v)]
chl <- ob("PHY_tchla"); cy <- ob("PHY_cyano")
if (length(chl) < 10) { message("too few surface chl-a observations (", length(chl), ") - no bloom prior"); quit(status = 2) }
obs_mean <- mean(chl); obs_p90 <- unname(quantile(chl, .9))
use_cy <- length(cy) >= 10 && mean(cy) > 0.05 * obs_mean
cy_p90 <- if (use_cy) unname(quantile(cy, .9)) else NA_real_
message(sprintf("%s: obs chl-a mean %.2f p90 %.2f (n=%d)%s", tag, obs_mean, obs_p90, length(chl),
                if (use_cy) sprintf("; cyano p90 %.3f (n=%d) used", cy_p90, length(cy)) else "; cyano not used"))

r <- runs[runs$run > 0, ]
accept <- function(fm, fp) {
  ok <- r$PHY_tchla__mean >= obs_mean / fm & r$PHY_tchla__mean <= obs_mean * fm &
        r$PHY_tchla__p90 >= obs_p90 / fp & r$PHY_tchla__p90 <= obs_p90 * fp
  if (use_cy) ok <- ok & r$PHY_cyano__p90 >= cy_p90 / 5 & r$PHY_cyano__p90 <= cy_p90 * 5
  ok & is.finite(ok)
}
fm <- FM; fp <- FP; ok <- accept(fm, fp)
for (i in 1:2) if (sum(ok) < MIN_RUNS) { fm <- fm * 1.4; fp <- fp * 1.4; ok <- accept(fm, fp); message(sprintf("  widened factors to %.2f / %.2f -> %d runs", fm, fp, sum(ok))) }
message(sprintf("accepted %d of %d runs (mean factor %.2f, p90 factor %.2f)", sum(ok), nrow(r), fm, fp))
summ <- data.frame(tag = tag, obs_mean = obs_mean, obs_p90 = obs_p90, use_cyano = use_cy, n_runs = nrow(r), n_accepted = sum(ok),
                   factor_mean = fm, factor_p90 = fp,
                   acc_mean_median = if (any(ok)) median(r$PHY_tchla__mean[ok]) else NA, acc_p90_median = if (any(ok)) median(r$PHY_tchla__p90[ok]) else NA)
write.csv(summ, file.path(d, "bloom_prior_summary.csv"), row.names = FALSE)
if (sum(ok) < MIN_RUNS) { message("fewer than ", MIN_RUNS, " plausible runs - no bloom prior written"); quit(status = 2) }

out <- par[match(r$run[ok], par$run), setdiff(names(par), "run"), drop = FALSE]
f <- file.path(d, "bloom_prior.csv"); write.csv(out, f, row.names = FALSE)
message("wrote ", f, " (", nrow(out), " parameter sets x ", ncol(out), " parameters)")
cat(f, "\n")

# Step 2 calibration: ensemble smoother with multiple data assimilation (ES-MDA, Emerick & Reynolds 2013)
# against the CSPS residuals defined in step2_common.R. Parameters are handled on a [0, 1] scale (log-scaled
# where the SA table says so), runs are executed in parallel.
#   Rscript calibration/step2/step2_ies.R
# Environment: S2_N (ensemble size, 80), S2_ITER (assimilation steps, 6), S2_NCORE (workers, 10), S2_SEED.
source("calibration/step2/step2_common.R")

N <- as.integer(Sys.getenv("S2_N", "80")); NIT <- as.integer(Sys.getenv("S2_ITER", "6"))
NW <- as.integer(Sys.getenv("S2_NCORE", "10")); SEED <- as.integer(Sys.getenv("S2_SEED", "7"))
ALPHA <- NIT                                     # constant inflation: sum(1/alpha) = 1
out_dir <- SA_DIR; dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- build once -----------------------------------------------------------------------------------
if (!file.exists(file.path(out_dir, "sa_base_aeme.rds"))) {
  b <- step2_build("sa_base")
  saveRDS(b$aeme, file.path(out_dir, "sa_base_aeme.rds"))
  saveRDS(step2_table(b), file.path(out_dir, "step2_table.rds"))
}
tab <- readRDS(file.path(out_dir, "step2_table.rds"))
vary <- which(tab$min < tab$max); k <- length(vary)
m <- length(STEP2_NAMES)
message("step 2: ", k, " parameters, ", m, " residuals, N = ", N, ", ", NIT, " steps, ", NW, " workers")
print(tab[vary, c("name", "group", "index", "value", "min", "max", "scale")], row.names = FALSE)

to_value <- function(u, r) {
  if (identical(r$scale, "log") && r$min * r$max > 0) {
    s <- sign(r$min); lo <- log(min(abs(r$min), abs(r$max))); hi <- log(max(abs(r$min), abs(r$max)))
    s * exp(lo + u * (hi - lo))
  } else r$min + u * (r$max - r$min)
}
to_unit <- function(v, r) {
  if (identical(r$scale, "log") && r$min * r$max > 0) {
    lo <- log(min(abs(r$min), abs(r$max))); hi <- log(max(abs(r$min), abs(r$max)))
    (log(abs(v)) - lo) / (hi - lo)
  } else (v - r$min) / (r$max - r$min)
}
param_for <- function(u) { p <- tab; for (j in seq_len(k)) p$value[vary[j]] <- to_value(u[j], tab[vary[j], ]); p }

# ---- workers --------------------------------------------------------------------------------------
cl <- parallel::makeCluster(NW, outfile = file.path("calibration", "logs", "step2_workers.log"))
on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
work_dir <- file.path(out_dir, "work"); dir.create(work_dir, showWarnings = FALSE)
parallel::clusterExport(cl, c("out_dir", "work_dir"), envir = environment())
parallel::clusterEvalQ(cl, {
  setwd("C:/Users/mooret/Git/rotorua-pc10")
  Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED = "FALSE")
  source("calibration/step2/step2_common.R")
  W_AEME <- readRDS(file.path(out_dir, "sa_base_aeme.rds"))
  W_PATH <- file.path(work_dir, paste0("w", Sys.getpid()))
  unlink(W_PATH, recursive = TRUE); dir.create(W_PATH, recursive = TRUE)
  file.copy(list.files(file.path(out_dir, "build", "sa_base"), full.names = TRUE), W_PATH, recursive = TRUE)
  TRUE
})
w_run <- function(p) tryCatch(step2_eval(W_AEME, W_PATH, p), error = function(e) { message("eval error: ", conditionMessage(e)); NULL })
parallel::clusterExport(cl, "w_run", envir = environment())

evaluate <- function(U, label) {
  t0 <- Sys.time()
  res <- parallel::clusterApplyLB(cl, lapply(seq_len(nrow(U)), function(i) param_for(U[i, ])), function(p) w_run(p))
  ok <- !vapply(res, is.null, logical(1))
  Ycal <- Yval <- matrix(NA_real_, nrow(U), m, dimnames = list(NULL, STEP2_NAMES))
  for (i in which(ok)) { Ycal[i, ] <- res[[i]]$cal; Yval[i, ] <- res[[i]]$val }
  z <- ok & rowSums(abs(Ycal), na.rm = TRUE) == 0      # all-zero residuals = no data scored (a truncated or killed run), not a perfect fit
  if (any(z)) { message(sum(z), " member(s) with all-zero residuals treated as failed"); Ycal[z, ] <- NA; Yval[z, ] <- NA; ok[z] <- FALSE }
  phi <- rowMeans(Ycal^2)
  message(sprintf("%s: %d/%d ok, phi median %.2f, best %.2f (%.1f min)", label, sum(ok), nrow(U),
                  median(phi, na.rm = TRUE), min(phi, na.rm = TRUE), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  list(Ycal = Ycal, Yval = Yval, phi = phi, ok = ok)
}

# ---- prior ensemble: Latin hypercube plus the baseline (calibrated alum-stage values, wind fixed at 1) ----
set.seed(SEED)
lhs <- function(n, k) sapply(seq_len(k), function(j) (sample.int(n) - runif(n)) / n)
U <- lhs(N, k)
# warm start (second pass): half the ensemble is the first-pass posterior re-expressed in the new ranges
warm <- Sys.getenv("S2_WARM_FROM", "")
if (nzchar(warm)) {
  old <- readRDS(warm); ot <- old$tab; ov <- old$vary
  Uo <- old$hist[[length(old$hist)]]$U
  Vo <- t(apply(Uo, 1, function(u) vapply(seq_len(k), function(j) to_value(u[j], ot[ov[j], ]), numeric(1))))
  Un <- t(apply(Vo, 1, function(v) vapply(seq_len(k), function(j) to_unit(v[j], tab[vary[j], ]), numeric(1))))
  n_warm <- floor(N / 2); U[seq_len(n_warm), ] <- pmin(pmax(Un[sample(nrow(Un), n_warm), ], 0), 1)
  message("warm start: ", n_warm, " members from ", warm)
}
u0 <- vapply(seq_len(k), function(j) to_unit(tab$value[vary[j]], tab[vary[j], ]), numeric(1))
U[1, ] <- pmin(pmax(u0, 0), 1)
hist <- list()

for (it in seq_len(NIT + 1)) {
  ev <- evaluate(U, sprintf("iteration %d", it - 1))
  hist[[it]] <- list(U = U, Ycal = ev$Ycal, Yval = ev$Yval, phi = ev$phi, ok = ev$ok)
  saveRDS(list(hist = hist, tab = tab, vary = vary, N = N, NIT = NIT), file.path(out_dir, "step2_ies.rds"))
  if (it > NIT) break
  good <- which(ev$ok)
  Y <- ev$Ycal
  # failed members take a random good member's response so the ensemble keeps its size
  bad <- which(!ev$ok); if (length(bad)) Y[bad, ] <- Y[sample(good, length(bad), replace = TRUE), ]
  dU <- scale(U, scale = FALSE); dY <- scale(Y, scale = FALSE)
  Cud <- crossprod(dU, dY) / (N - 1); Cdd <- crossprod(dY) / (N - 1)
  pert <- matrix(rnorm(N * m, 0, sqrt(ALPHA)), N, m)           # observations are 0 (residuals are model - obs)
  Kmat <- Cud %*% solve(Cdd + ALPHA * diag(m), t(pert - Y))
  U <- U + t(Kmat)
  U <- pmin(pmax(U, 0), 1)
}

# ---- results --------------------------------------------------------------------------------------
last <- hist[[NIT + 1]]
best <- which.min(last$phi)
tab_best <- param_for(last$U[best, ])
saveRDS(list(best = tab_best, U_best = last$U[best, ], phi_best = last$phi[best],
             baseline_phi = hist[[1]]$phi[1], baseline_Ycal = hist[[1]]$Ycal[1, ], baseline_Yval = hist[[1]]$Yval[1, ],
             best_Ycal = last$Ycal[best, ], best_Yval = last$Yval[best, ]),
        file.path(out_dir, "step2_best.rds"))
utils::write.csv(tab_best[, c("model", "file", "name", "value", "min", "max", "group", "index")],
                 file.path(out_dir, "step2_best_params.csv"), row.names = FALSE)
message("best member phi (cal) = ", round(last$phi[best], 3), " vs baseline ", round(hist[[1]]$phi[1], 3))

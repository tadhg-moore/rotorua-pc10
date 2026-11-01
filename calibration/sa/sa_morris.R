# Morris elementary-effects screening with CSPS outputs, run in parallel.
#   Rscript calibration/sa/sa_morris.R            (needs sa_smoke.R to have been run: sa_base_aeme.rds, sa_table.rds)
# Environment: SA_R (trajectories, default 10), SA_NCORE (workers, default 10), SA_SEED, SA_LEVELS (default 6)
# Resumable: each run's outputs are written to calibration/sa_runs/results/run_<id>.rds.
source("calibration/sa/sa_common.R")

R_TRAJ <- as.integer(Sys.getenv("SA_R", "10"))
NW     <- as.integer(Sys.getenv("SA_NCORE", "10"))
SEED   <- as.integer(Sys.getenv("SA_SEED", "20261018"))
P_LEV  <- as.integer(Sys.getenv("SA_LEVELS", "6"))
DELTA  <- P_LEV / (2 * (P_LEV - 1))

tab <- readRDS(file.path(SA_DIR, "sa_table.rds"))
vary <- which(tab$min < tab$max)
k <- length(vary)
message("SA: ", k, " parameters, ", R_TRAJ, " trajectories -> ", R_TRAJ * (k + 1), " runs, ", NW, " workers")

# ---- design ------------------------------------------------------------------------------------
set.seed(SEED)
make_traj <- function() {
  grid <- (0:(P_LEV - 1)) / (P_LEV - 1)
  x <- sample(grid, k, replace = TRUE)
  pts <- matrix(NA_real_, k + 1, k); pts[1, ] <- x
  ord <- sample(k)
  for (s in seq_len(k)) {
    j <- ord[s]
    up <- x[j] + DELTA <= 1 + 1e-9; dn <- x[j] - DELTA >= -1e-9
    x[j] <- if (up && (!dn || runif(1) < 0.5)) x[j] + DELTA else x[j] - DELTA
    pts[s + 1, ] <- x
  }
  list(pts = pts, ord = ord)
}
trajs <- lapply(seq_len(R_TRAJ), function(i) make_traj())
design <- do.call(rbind, lapply(seq_along(trajs), function(i) {
  cbind(traj = i, step = 0:k, param = c(NA, trajs[[i]]$ord), trajs[[i]]$pts)
}))
design <- cbind(run = seq_len(nrow(design)), design)
saveRDS(list(design = design, vary = vary, delta = DELTA, tab = tab), file.path(SA_DIR, "sa_design.rds"))

to_value <- function(u, r) {
  if (identical(r$scale, "log") && r$min * r$max > 0) {
    s <- sign(r$min); lo <- log(min(abs(r$min), abs(r$max))); hi <- log(max(abs(r$min), abs(r$max)))
    s * exp(lo + u * (hi - lo))
  } else r$min + u * (r$max - r$min)
}
param_for_run <- function(i) {
  p <- tab
  u <- design[i, -(1:4)]
  for (jj in seq_along(vary)) p$value[vary[jj]] <- to_value(u[jj], tab[vary[jj], ])
  p
}

# ---- workers ----------------------------------------------------------------------------------
res_dir <- file.path(SA_DIR, "results"); dir.create(res_dir, showWarnings = FALSE)
work_dir <- file.path(SA_DIR, "work"); dir.create(work_dir, showWarnings = FALSE)
todo <- which(!file.exists(file.path(res_dir, sprintf("run_%05d.rds", design[, "run"]))))
message(length(todo), " runs to do")

cl <- parallel::makeCluster(NW, outfile = file.path("calibration", "logs", "sa_workers.log"))
on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
parallel::clusterExport(cl, c("SA_DIR", "work_dir", "res_dir"), envir = environment())
parallel::clusterEvalQ(cl, {
  setwd("C:/Users/mooret/Git/rotorua-pc10")
  Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED = "FALSE")
  source("calibration/sa/sa_common.R")
  id <- Sys.getpid()
  W_AEME <- readRDS(file.path(SA_DIR, "sa_base_aeme.rds"))
  W_PATH <- file.path(work_dir, paste0("w", id))
  unlink(W_PATH, recursive = TRUE)
  dir.create(W_PATH, recursive = TRUE)
  file.copy(list.files(file.path(SA_DIR, "build", "sa_base"), full.names = TRUE), W_PATH, recursive = TRUE)
  TRUE
})
one_run <- function(i, p) {
  f <- file.path(res_dir, sprintf("run_%05d.rds", i))
  t0 <- Sys.time()
  out <- tryCatch(sa_eval(W_AEME, W_PATH, p), error = function(e) { message("run ", i, " error: ", conditionMessage(e)); NULL })
  saveRDS(list(run = i, out = out, secs = as.numeric(difftime(Sys.time(), t0, units = "secs"))), f)
  message(format(Sys.time(), "%H:%M:%S"), " run ", i, if (is.null(out)) " FAILED" else " ok",
          " (", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min)")
  i
}
parallel::clusterExport(cl, "one_run", envir = environment())
jobs <- lapply(todo, function(i) list(i = i, p = param_for_run(i)))
invisible(parallel::clusterApplyLB(cl, jobs, function(j) one_run(j$i, j$p)))
message("SA runs complete")

# Morris sensitivity analysis on the latest setup: joint-stage (j) best member as the base point, V1 inflows, flood x 0.5 base (base_Q50), alum on, 2005-01 to 2022-06.
# Every continuous parameter of the model that can be varied through the parameter table is sampled (about 140), with many trajectories so mu* can be quantified.
#   Rscript calibration/sa/sa_latest.R smoke      build, evaluate the base point once, print timing and outputs
#   Rscript calibration/sa/sa_latest.R            run the design (resumable: finished runs are skipped)
# Environment: SA_R (trajectories, 30), SA_NCORE (workers, 10), SA_SEED, SA_LEVELS (6). Output: calibration/sa_latest/
# Trajectories are generated in sequence from the seed, so raising SA_R later extends the design and keeps the finished runs.
args <- commandArgs(TRUE); mode <- if (length(args)) args[1] else "run"
Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED = "FALSE", S2_STAGE = "p", S2_JOINT = "1", S2_ALUM = "1", S2_ALP = "1", S2_TPTERMS = "1",
           S2_BASE = "calibration/alum/stream/base_Q50.rds", S2_START = "2005-01-01", S2_STOP = "2022-06-29", S2_DIR = "calibration/sa_latest",
           S2_ALUM_FROM = "calibration/step2o3/runs/step2_best.rds", S2_OVERLAY = "calibration/step2o/runs/step2_best.rds,calibration/step2q2/runs/step2_best.rds,calibration/step2o3/runs/step2_best.rds")
.libPaths(c(.libPaths(), "C:/Users/mooret/AppData/Local/R/win-library/4.5"))
source("calibration/step2/step2_common.R")
source("calibration/sa/sa_latest_eval.R")
R_TRAJ <- as.integer(Sys.getenv("SA_R", "30")); NW <- as.integer(Sys.getenv("SA_NCORE", "10")); SEED <- as.integer(Sys.getenv("SA_SEED", "20261030")); P_LEV <- as.integer(Sys.getenv("SA_LEVELS", "6"))
DELTA <- P_LEV / (2 * (P_LEV - 1)); out_dir <- SA_DIR; dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- parameter table: j best member as the base point, ranges from sa_spec() plus extras ---------------------------------------------------------------------
build_table <- function(b) {
  t0 <- step2_table(b); jb <- readRDS("calibration/step2j/runs/step2_best.rds")$best
  h <- match(.key2(t0), .key2(jb)); u <- !is.na(h); t0$value[u] <- jb$value[h[u]]; cat("base point: joint-stage best member,", sum(u), "of", nrow(t0), "rows matched\n")
  cols <- c("model", "file", "name", "value", "min", "max", "group", "index", "scale", "theme")
  extra <- local({ S <- list(); add <- function(name, min, max, scale = "log", index = NA_real_, theme = "light") S[[length(S) + 1]] <<- data.frame(model = "glm_aed", file = "aed.nml", name = name, group = NA_character_, index = index, min = min, max = max, scale = scale, theme = theme, stringsAsFactors = FALSE)
    for (z in 1:4) add("aed_sed_const2d/fsed_nit", NA, NA, "log", z, "sediment")
    add("aed_noncohesive/epsilon", 1e-6, 3e-4); add("aed_noncohesive/tau_0", 0.02, 0.5, index = 1); add("aed_noncohesive/tau_0", 0.02, 0.5, index = 2)
    add("aed_noncohesive/w_ss", -3, -0.05, index = 1); add("aed_noncohesive/w_ss", -0.3, -0.003, index = 2); add("aed_noncohesive/Ke_ss", 0.02, 0.15, index = 1); add("aed_noncohesive/Ke_ss", 0.02, 0.15, index = 2)
    do.call(rbind, S) })
  spec0 <- sa_spec; sa_spec <<- function() rbind(spec0(), extra); SA_REL[["aed_sed_const2d/fsed_nit"]] <<- c(5, 0.2)
  cfg_glm <- AEME::configuration(b$aeme)$glm_aed
  ph <- utils::read.csv(list.files(b$path, "aed_phyto_pars.csv", recursive = TRUE, full.names = TRUE)[1], header = FALSE, stringsAsFactors = FALSE); ph[] <- lapply(ph, function(z) gsub("'", "", z)); names(ph) <- ph[1, ]; ph <- ph[-1, ]
  tab <- sa_param_table(t0[, cols], cfg_glm, ph)
  # weir and sediment temperature: absolute / relative ranges around the base value
  rr <- function(nm, idx = NULL, lo, hi, rel = TRUE) for (i in which(tab$name == nm & (is.null(idx) | tab$index %in% idx))) { v <- tab$value[i]; tab$min[i] <<- if (rel) lo * v else v + lo; tab$max[i] <<- if (rel) hi * v else v + hi; tab$scale[i] <<- "lin"; tab$theme[i] <<- "physics" }
  rr("morphometry/crest_elev", NULL, -0.1, 0.1, FALSE); rr("outflow/crest_factor", NULL, 0.5, 1.5); rr("sediment/sed_temp_amplitude", NULL, 0.5, 1.5)
  rr("sediment/sed_temp_mean", NULL, -3, 3, FALSE); rr("sediment/sed_temp_mean_zoffset", NULL, -3, 3, FALSE)
  tab$min <- pmin(tab$min, tab$value); tab$max <- pmax(tab$max, tab$value); tab }

if (!file.exists(file.path(out_dir, "sa_base_aeme.rds"))) { b <- step2_build("sa_base"); saveRDS(b$aeme, file.path(out_dir, "sa_base_aeme.rds")); saveRDS(build_table(b), file.path(out_dir, "sa_table.rds")) }
b <- list(aeme = readRDS(file.path(out_dir, "sa_base_aeme.rds")), path = file.path(out_dir, "build", "sa_base")); tab <- readRDS(file.path(out_dir, "sa_table.rds"))
vary <- which(tab$min < tab$max); k <- length(vary); cat(k, "parameters varied\n"); print(table(tab$theme[vary]))

if (mode == "smoke") { t0 <- Sys.time(); o <- sa_latest_eval(b$aeme, b$path, tab); cat("evaluation:", round(difftime(Sys.time(), t0, units = "mins"), 2), "min\n")
  if (is.null(o)) stop("base evaluation failed"); saveRDS(o, file.path(out_dir, "sa_baseline_out.rds")); cat(length(o), "outputs,", sum(!is.finite(o)), "not finite\n"); print(signif(o[grep("^(phi|x_)", names(o))], 4)); quit(save = "no") }

# ---- design -----------------------------------------------------------------------------------------------------------------------------------------------
set.seed(SEED)
make_traj <- function() { grid <- (0:(P_LEV - 1)) / (P_LEV - 1); x <- sample(grid, k, replace = TRUE); pts <- matrix(NA_real_, k + 1, k); pts[1, ] <- x; ord <- sample(k)
  for (s in seq_len(k)) { j <- ord[s]; up <- x[j] + DELTA <= 1 + 1e-9; dn <- x[j] - DELTA >= -1e-9; x[j] <- if (up && (!dn || runif(1) < 0.5)) x[j] + DELTA else x[j] - DELTA; pts[s + 1, ] <- x }
  list(pts = pts, ord = ord) }
trajs <- lapply(seq_len(R_TRAJ), function(i) make_traj())
design <- do.call(rbind, lapply(seq_along(trajs), function(i) cbind(traj = i, step = 0:k, param = c(NA, trajs[[i]]$ord), trajs[[i]]$pts))); design <- cbind(run = seq_len(nrow(design)), design)
saveRDS(list(design = design, vary = vary, delta = DELTA, tab = tab), file.path(out_dir, "sa_design.rds"))
to_value <- function(u, r) if (identical(r$scale, "log") && r$min * r$max > 0) { s <- sign(r$min); lo <- log(min(abs(r$min), abs(r$max))); hi <- log(max(abs(r$min), abs(r$max))); s * exp(lo + u * (hi - lo)) } else r$min + u * (r$max - r$min)
param_for_run <- function(i) { p <- tab; u <- design[i, -(1:4)]; for (jj in seq_along(vary)) p$value[vary[jj]] <- to_value(u[jj], tab[vary[jj], ]); p }

# ---- workers ----------------------------------------------------------------------------------------------------------------------------------------------
res_dir <- file.path(out_dir, "results"); dir.create(res_dir, showWarnings = FALSE); work_dir <- file.path(out_dir, "work"); dir.create(work_dir, showWarnings = FALSE)
todo <- which(!file.exists(file.path(res_dir, sprintf("run_%05d.rds", design[, "run"])))); message(length(todo), " of ", nrow(design), " runs to do")
cl <- parallel::makeCluster(NW, outfile = file.path("calibration", "logs", "sa_latest_workers.log")); on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
parallel::clusterExport(cl, c("out_dir", "work_dir", "res_dir"), envir = environment())
parallel::clusterEvalQ(cl, { setwd("C:/Users/mooret/Git/rotorua-pc10"); .libPaths(c(.libPaths(), "C:/Users/mooret/AppData/Local/R/win-library/4.5"))
  source("calibration/step2/step2_common.R"); source("calibration/sa/sa_latest_eval.R")
  W_AEME <- readRDS(file.path(out_dir, "sa_base_aeme.rds")); W_PATH <- file.path(work_dir, paste0("w", Sys.getpid())); unlink(W_PATH, recursive = TRUE); dir.create(W_PATH, recursive = TRUE)
  file.copy(list.files(file.path(out_dir, "build", "sa_base"), full.names = TRUE), W_PATH, recursive = TRUE); TRUE })
one_run <- function(i, p) { f <- file.path(res_dir, sprintf("run_%05d.rds", i)); t0 <- Sys.time()
  o <- tryCatch(sa_latest_eval(W_AEME, W_PATH, p), error = function(e) { message("run ", i, " error: ", conditionMessage(e)); NULL })
  saveRDS(list(run = i, out = o, secs = as.numeric(difftime(Sys.time(), t0, units = "secs"))), f)
  message(format(Sys.time(), "%H:%M:%S"), " run ", i, if (is.null(o)) " FAILED" else " ok", " (", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min)"); i }
parallel::clusterExport(cl, "one_run", envir = environment())
invisible(parallel::clusterApplyLB(cl, lapply(todo, function(i) list(i = i, p = param_for_run(i))), function(j) one_run(j$i, j$p)))
message("SA runs complete")

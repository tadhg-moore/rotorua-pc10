# Morris elementary effects per parameter and CSPS output, for one SA variant.
#   SA_VARIANT=alum   Rscript calibration/sa/sa_analyse.R
#   SA_VARIANT=noalum Rscript calibration/sa/sa_analyse.R
# Writes <SA_DIR>/sa_effects.csv (long: parameter x output) and sa_runs_table.csv (run x output).
SA_VARIANT <- Sys.getenv("SA_VARIANT", "alum")
SA_DIR <- paste0(if (identical(SA_VARIANT, "alum")) "calibration/sa_runs" else paste0("calibration/sa_runs_", SA_VARIANT), Sys.getenv("SA_TAG", ""))
suppressMessages(library(dplyr))

d <- readRDS(file.path(SA_DIR, "sa_design.rds"))
des <- d$design; tab <- d$tab; vary <- d$vary; delta <- d$delta
f <- list.files(file.path(SA_DIR, "results"), full.names = TRUE)
res <- lapply(f, readRDS)
ids <- vapply(res, `[[`, 1L, "run")
outs <- lapply(res, `[[`, "out")
ok <- !vapply(outs, is.null, logical(1))
message(length(res), " runs read, ", sum(!ok), " failed (", round(100 * mean(!ok), 1), "%)")
onames <- sort(unique(unlist(lapply(outs[ok], names))))
Y <- matrix(NA_real_, nrow(des), length(onames), dimnames = list(NULL, onames))
for (i in which(ok)) Y[ids[i], names(outs[[i]])] <- outs[[i]]
Y[!is.finite(Y)] <- NA
write.csv(cbind(run = seq_len(nrow(des)), Y), file.path(SA_DIR, "sa_runs_table.csv"), row.names = FALSE)

plab <- ifelse(is.na(tab$group[vary]), "", paste0(" (", tab$group[vary], ")"))
zlab <- ifelse(is.na(tab$group[vary]) & !is.na(tab$index[vary]), paste0(" z", tab$index[vary]), "")
plabel <- paste0(sub("^.*/", "", tab$name[vary]), plab, zlab)
ptheme <- tab$theme[vary]

# elementary effects: consecutive runs within a trajectory differ in one parameter
U <- des[, -(1:4), drop = FALSE]
rows <- list()
for (r in 2:nrow(des)) {
  if (des[r, "step"] == 0) next
  j <- des[r, "param"]
  du <- U[r, j] - U[r - 1, j]
  ee <- (Y[r, ] - Y[r - 1, ]) / du
  rows[[length(rows) + 1]] <- data.frame(run = r, traj = des[r, "traj"], pidx = j, output = onames, ee = ee, stringsAsFactors = FALSE)
}
E <- do.call(rbind, rows)
E <- E[is.finite(E$ee), ]
eff <- E |> group_by(pidx, output) |>
  summarise(n = n(), mu = mean(ee), mu_star = mean(abs(ee)), sigma = sd(ee), .groups = "drop") |>
  mutate(parameter = plabel[pidx], theme = ptheme[pidx])

# scale: signatures by their tolerance (interannual sd); other outputs by their sd across runs
tg <- read.csv("calibration/csps_targets.csv")
scl <- apply(Y, 2, sd, na.rm = TRUE)
scl[match(tg$signature, names(scl), nomatch = 0)] <- tg$tolerance[tg$signature %in% names(scl)]
eff$scale <- scl[eff$output]
eff$mu_star_scaled <- eff$mu_star / eff$scale      # output change (in tolerance units) across the full parameter range
write.csv(eff, file.path(SA_DIR, "sa_effects.csv"), row.names = FALSE)

# quick console summary: top 6 parameters for each signature
sig <- grep("^(mix|anox|load|chl|sys)_", onames, value = TRUE)
for (o in sig) {
  t <- eff[eff$output == o, ] |> arrange(desc(mu_star_scaled)) |> head(6)
  cat(sprintf("%-26s %s\n", o, paste0(t$parameter, " [", round(t$mu_star_scaled, 2), "]", collapse = ", ")))
}

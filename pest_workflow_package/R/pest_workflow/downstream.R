# Downstream-lake support for calibration and forward runs.
#
# A downstream lake (Slipper<-Spectacle, Pehiakura Big<-Pehiakura Small,
# Kereta<-Te Kanae, Kuwakatai<-Rototoa) receives its parent lake's outflow as
# an inflow ("outflow_inflow"). The cached aeme_baseline_downstream targets
# carry an OLD-pipeline parent outflow; here it is swapped for the parent's
# outflow from the current calibrated ensemble
# (pest_workflow/<parent>/outflow/<scenario>_outflow.csv, written by
# run_lake_ensemble()).

#' parent lake tag (e.g. "LID50270_rototoa") for a downstream LID
parent_tag_for <- function(lid_down, lake_metadata = targets::tar_read(lake_metadata),
                           checkpoint_root = "staged-calibration-v4/checkpoints") {
  down <- lake_metadata[lake_metadata$lernzmp_id == lid_down, ]
  stopifnot(nrow(down) == 1, !is.na(down$lake_upstream))
  up <- lake_metadata[lake_metadata$lake == down$lake_upstream, ]
  stopifnot(nrow(up) == 1)
  # tag = <LID>_<lowercase lake name>, matching the checkpoint/dir naming
  paste0(up$lernzmp_id, "_", tolower(gsub("[^A-Za-z0-9]", "", up$lake)))
}

#' Downstream Aeme with the parent's CURRENT outflow as inflow.
#' @param parent_outflow_csv file written by run_lake_ensemble() for the parent
build_downstream_aeme <- function(lid_down, parent_outflow_csv,
                                  aeme_downstream = targets::tar_read(aeme_baseline_downstream)) {
  if (!file.exists(parent_outflow_csv))
    stop("parent outflow not found (run the parent's ensemble first): ", parent_outflow_csv,
         call. = FALSE)
  ids <- vapply(aeme_downstream, function(a) tryCatch(AEME::lake(a)$id, error = function(e) NA_character_),
                character(1))
  hit <- which(ids == lid_down)
  if (length(hit) == 0) stop("no aeme_baseline_downstream entry for ", lid_down, call. = FALSE)
  a <- aeme_downstream[[hit[1]]]
  of <- utils::read.csv(parent_outflow_csv, stringsAsFactors = FALSE)
  of <- of[, setdiff(names(of), c("model", "scenario"))]
  a <- AEME::remove_inflow(a, inflow_id = "outflow_inflow")
  a <- AEME::add_inflow(a, inflow = of, inflow_id = "outflow_inflow")

  # The cached downstream objects carry no data path (upstream ones do, e.g.
  # aeme/LID50270_rototoa). v6 names checkpoints from basename(get_lake_dir()),
  # so give the lake the same <LID>_<lowercase name> directory convention.
  if (is.null(AEME::get_aeme_path(a, require = FALSE))) {
    nm <- tolower(gsub("[^A-Za-z0-9]", "", AEME::lake(a)$name))
    cfg <- AEME::configuration(a)
    dir.create(file.path("aeme", paste0(lid_down, "_", nm)), showWarnings = FALSE)
    cfg$path <- file.path(normalizePath("aeme", winslash = "\\"), paste0(lid_down, "_", nm))
    AEME::configuration(a) <- cfg
  }
  a
}

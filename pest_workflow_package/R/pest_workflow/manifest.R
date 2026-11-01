# Manifest helpers for the PEST calibrate -> ensemble -> scenario -> dashboard
# workflow. Every step writes a manifest.json recording exactly which inputs
# (path + sha256) it consumed, so stale outputs can be detected rather than
# silently reused.

PEST_WF_ROOT <- Sys.getenv("PEST_WF_ROOT", "pest_workflow")   # env override: A/B runs write elsewhere

wf_dir <- function(lake_tag, ..., create = TRUE) {
  d <- file.path(PEST_WF_ROOT, lake_tag, ...)
  if (create && !dir.exists(d)) dir.create(d, recursive = TRUE)
  d
}

file_sha256 <- function(path) {
  stopifnot(file.exists(path))
  con <- file(path, "rb")
  on.exit(close(con))
  h <- openssl::sha256(con)
  paste(as.character(unclass(h)), collapse = "")
}

#' Describe a set of input files as list(path, mtime, sha256)
describe_inputs <- function(paths) {
  paths <- paths[!is.na(paths)]
  lapply(stats::setNames(paths, basename(paths)), function(p) {
    list(path = normalizePath(p, winslash = "/"),
         mtime = format(file.mtime(p), "%Y-%m-%dT%H:%M:%S"),
         sha256 = file_sha256(p))
  })
}

#' Write a manifest next to an output.
#' @param out_dir directory the manifest lives in
#' @param step step name, e.g. "posterior", "ens_baseline"
#' @param inputs named list from describe_inputs()
#' @param extra named list of extra facts (n_members, cal_id, ...)
write_manifest <- function(out_dir, step, inputs, extra = list(),
                           file = "manifest.json") {
  m <- c(list(step = step,
              created = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
              R_version = as.character(getRversion()),
              aemetools = as.character(utils::packageVersion("aemetools")),
              AEME = as.character(utils::packageVersion("AEME")),
              inputs = inputs),
         extra)
  jsonlite::write_json(m, file.path(out_dir, file), auto_unbox = TRUE,
                       pretty = TRUE, null = "null")
  invisible(m)
}

read_manifest <- function(out_dir, file = "manifest.json") {
  f <- file.path(out_dir, file)
  if (!file.exists(f)) return(NULL)
  jsonlite::read_json(f, simplifyVector = FALSE)
}

#' TRUE when every input recorded in `manifest` still has the same sha256
manifest_is_current <- function(manifest) {
  if (is.null(manifest)) return(FALSE)
  all(vapply(manifest$inputs, function(i) {
    file.exists(i$path) && identical(file_sha256(i$path), i$sha256)
  }, logical(1)))
}

#' Stop unless a lake's manifest for `step` exists and its inputs are unchanged
require_current_manifest <- function(lake_tag, ..., step) {
  m <- read_manifest(wf_dir(lake_tag, ..., create = FALSE))
  if (is.null(m)) stop(sprintf("[%s] no %s manifest - run that step first",
                               lake_tag, step), call. = FALSE)
  if (!manifest_is_current(m)) {
    stop(sprintf("[%s] %s manifest is STALE (inputs changed since it was built)",
                 lake_tag, step), call. = FALSE)
  }
  m
}

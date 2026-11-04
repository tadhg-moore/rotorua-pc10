Sys.setenv(RENV_PROJECT = normalizePath(".."))
source("../renv/activate.R")

# GLM_VERSION is pinned in .github/workflows/publish.yaml ("latest" when run locally)
glm_version <- Sys.getenv("GLM_VERSION", "latest")
AEME::install_glm_aed(version = glm_version)

# Run the pipeline and record whether it produced any new data, so the workflow only saves a
# new targets cache when something changed (the store is large).
store_hash <- function() {
  m <- tryCatch(targets::tar_meta(fields = "data", store = "../_targets/"), error = function(e) NULL)
  if (is.null(m) || !nrow(m)) return(stats::setNames(character(), character()))
  stats::setNames(m$data, m$name)
}
before <- store_hash()
# sync_onedrive_*() appends a line here for every folder/file it actually downloads (skips are
# logged too, but as "unchanged"), so we know whether the OneDrive data cache needs re-saving.
sync_log <- tempfile("onedrive_sync_", fileext = ".log")
Sys.setenv(ONEDRIVE_SYNC_LOG = sync_log)
t0 <- Sys.time()
targets::tar_make()
after <- store_hash()
changed <- !identical(before[sort(names(before))], after[sort(names(after))])
log_lines <- if (file.exists(sync_log)) readLines(sync_log, warn = FALSE) else character()
downloaded <- any(grepl("downloading", log_lines, fixed = TRUE))
message(sprintf("targets pipeline: %.1f min, store %s, OneDrive: %d downloaded / %d unchanged",
                as.numeric(difftime(Sys.time(), t0, units = "mins")),
                if (changed) "changed" else "unchanged",
                sum(grepl("downloading", log_lines, fixed = TRUE)),
                sum(grepl("unchanged", log_lines, fixed = TRUE))))
out <- Sys.getenv("GITHUB_OUTPUT")
if (nzchar(out)) {
  cat(sprintf("changed=%s\nonedrive_downloaded=%s\n", tolower(changed), tolower(downloaded)),
      file = out, append = TRUE)
}

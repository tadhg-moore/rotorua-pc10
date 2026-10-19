Sys.setenv(RENV_PROJECT = normalizePath(".."))
source("../renv/activate.R")

# GLM_VERSION is pinned in .github/workflows/publish.yaml ("latest" when run locally)
glm_version <- Sys.getenv("GLM_VERSION", "latest")
AEME::install_glm_aed(version = glm_version)

# Run the pipeline and record whether it produced any new data, so the workflow only saves a
# new targets cache when something changed (the store is large).
store_hash <- function() {
  m <- tryCatch(targets::tar_meta(fields = "data", store = "../_targets/"), error = function(e) NULL)
  if (is.null(m) || !nrow(m)) return(character())
  stats::setNames(m$data, m$name)
}
before <- store_hash()
t0 <- Sys.time()
targets::tar_make()
after <- store_hash()
changed <- !identical(before[order(names(before))], after[order(names(after))])
message(sprintf("targets pipeline: %.1f min, store %s", as.numeric(difftime(Sys.time(), t0, units = "mins")),
                if (changed) "changed" else "unchanged"))
out <- Sys.getenv("GITHUB_OUTPUT")
if (nzchar(out)) cat(sprintf("changed=%s\n", tolower(changed)), file = out, append = TRUE)

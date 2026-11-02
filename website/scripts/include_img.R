# Drop-in for knitr::include_graphics() that does not stop the render when a
# figure is missing (report images are local-only assets synced from OneDrive).
# Missing files give a visible "Image missing" note and a warning instead.
include_img <- function(path, ...) {
  missing <- path[!file.exists(path)]
  if (length(missing) == 0) return(knitr::include_graphics(path, ...))
  warning("Image missing: ", paste(missing, collapse = ", "), call. = FALSE)
  note <- sprintf("**Image missing:** `%s`", missing)
  present <- path[file.exists(path)]
  out <- if (length(present)) knitr::include_graphics(present, ...) else NULL
  knitr::asis_output(paste(c(note, "", if (!is.null(out)) paste0("![](", present, ")")),
                           collapse = "\n"))
}

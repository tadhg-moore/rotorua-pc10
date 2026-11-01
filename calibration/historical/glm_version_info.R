# Prints the AEME version and the GLM-AED executable in use (path, version, size, date, sha256).
suppressMessages(library(AEME)); d <- packageDescription("AEME"); cat("AEME", d$Version, d$RemoteRef, substr(d$RemoteSha, 1, 7), "built", d$Built, "\n")
e <- tryCatch(AEME::glm_exe_path(), error = function(x) NA); cat("glm exe:", e, "\n"); if (!is.na(e) && file.exists(e)) cat("size", file.size(e), "| modified", format(file.mtime(e)), "| sha256", digest::digest(e, algo = "sha256", file = TRUE), "\n")
cat("version:", tryCatch(AEME::get_glm_aed_version(), error = function(x) conditionMessage(x)), "| installed:", paste(tryCatch(AEME::list_glm_versions(), error = function(x) NA), collapse = ", "), "\n")
if (!is.na(e) && file.exists(e)) cat(tryCatch(system2(e, "--version", stdout = TRUE, stderr = TRUE), error = function(x) "no --version output"), sep = "\n")

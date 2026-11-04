#' Open a Microsoft365R business OneDrive session using the encrypted
#' Azure token stored in ONEDRIVE_TOKEN_ENCRYPTED / ONEDRIVE_TOKEN_PASSWORD /
#' ONEDRIVE_TOKEN_HASH env vars (see create_onedrive_token.R for how the
#' token is produced).
get_onedrive_session <- function() {
  bundle    <- openssl::base64_decode(Sys.getenv("ONEDRIVE_TOKEN_ENCRYPTED"))
  iv        <- bundle[1:16]
  encrypted <- bundle[17:length(bundle)]
  key       <- openssl::sha256(charToRaw(Sys.getenv("ONEDRIVE_TOKEN_PASSWORD")))
  token_raw <- openssl::aes_cbc_decrypt(encrypted, key = key, iv = iv)

  cache_dir <- AzureAuth::AzureR_dir()
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  writeBin(token_raw, file.path(cache_dir, Sys.getenv("ONEDRIVE_TOKEN_HASH")))

  token <- AzureAuth::load_azure_token(hash = Sys.getenv("ONEDRIVE_TOKEN_HASH"))
  Microsoft365R::get_business_onedrive(token = token)
}

#' Content fingerprint for row `i` of a `list_items(info = "all")` data frame.
#' Uses every content hash Graph supplies (quickXorHash on OneDrive for
#' Business, sha1/sha256 elsewhere) and only falls back to eTag when there is
#' none, so metadata-only edits don't count as changes.
od_row_fingerprint <- function(x, i) {
  f <- x[["file"]]
  if (is.data.frame(f) && is.data.frame(f[["hashes"]])) {
    h <- unlist(f[["hashes"]][i, , drop = FALSE])
    h <- h[!is.na(h)]
    if (length(h)) return(paste(sort(paste(names(h), h, sep = "=")), collapse = ";"))
  }
  if (!is.null(x[["eTag"]])) x[["eTag"]][i] else NA_character_
}

#' Remote manifest of a OneDrive file or folder (recursive): one row per file
#' with its relative path, size and content fingerprint. Cheap (metadata only,
#' no downloads). `od` is injectable for testing.
onedrive_manifest <- function(src, od = get_onedrive_session()) {
  empty <- data.frame(path = character(), size = numeric(), hash = character(),
                      stringsAsFactors = FALSE)
  walk <- function(path, prefix) {
    x <- od$get_item(path)$list_items(info = "all")
    out <- list(empty)
    for (i in seq_len(nrow(x))) {
      rel <- if (nzchar(prefix)) paste(prefix, x$name[i], sep = "/") else x$name[i]
      if (isTRUE(x$isdir[i])) {
        out[[length(out) + 1]] <- walk(paste(path, x$name[i], sep = "/"), rel)
      } else {
        out[[length(out) + 1]] <- data.frame(
          path = rel, size = as.numeric(x$size[i]),
          hash = od_row_fingerprint(x, i), stringsAsFactors = FALSE)
      }
    }
    do.call(rbind, out)
  }
  item <- od$get_item(src)
  m <- if (is.null(item$properties$folder)) {
    # single file: take its own properties
    p <- item$properties
    h <- unlist(p$file$hashes)
    data.frame(path = p$name, size = as.numeric(p$size),
               hash = if (length(h)) paste(sort(paste(names(h), h, sep = "=")), collapse = ";")
                      else if (!is.null(p$eTag)) p$eTag else NA_character_,
               stringsAsFactors = FALSE)
  } else {
    walk(src, "")
  }
  m <- m[order(m$path), , drop = FALSE]
  rownames(m) <- NULL
  m
}

#' Single SHA-256 key for a manifest; changes iff any file is added, removed,
#' renamed or has different content.
onedrive_manifest_key <- function(manifest) {
  txt <- paste(manifest$path, manifest$size, manifest$hash, sep = "\t",
               collapse = "\n")
  paste(unclass(openssl::sha256(charToRaw(txt))), collapse = "")
}

# Stamps record the manifest key last downloaded to a destination. They live in
# a separate (git-ignored, CI-cached) dir rather than inside `dest`, so synced
# folders stay clean. A stale or missing stamp just means "download again".
od_stamp_path <- function(src, dest) {
  slug <- gsub("[^A-Za-z0-9]+", "_", src)
  root <- Sys.getenv("ONEDRIVE_STAMP_DIR", here::here(".onedrive_stamps"))
  file.path(root, paste0(slug, ".sha256"))
}

od_log <- function(...) {
  msg <- paste0(...)
  message(msg)
  log_file <- Sys.getenv("ONEDRIVE_SYNC_LOG")
  if (nzchar(log_file)) cat(msg, "\n", file = log_file, append = TRUE, sep = "")
}

# TRUE when `dest` is already current for `key`: stamp matches and the local
# copy exists. ONEDRIVE_FORCE_SYNC=true bypasses the check.
od_is_current <- function(src, dest, key) {
  if (identical(tolower(Sys.getenv("ONEDRIVE_FORCE_SYNC")), "true")) return(FALSE)
  stamp <- od_stamp_path(src, dest)
  file.exists(stamp) && file.exists(dest) &&
    identical(trimws(readLines(stamp, warn = FALSE)[1]), key)
}

od_write_stamp <- function(src, dest, key) {
  stamp <- od_stamp_path(src, dest)
  dir.create(dirname(stamp), recursive = TRUE, showWarnings = FALSE)
  writeLines(key, stamp)
}

#' Download a single file from OneDrive to `dest`, skipping the download when
#' the remote content hash matches the last download and `dest` still exists.
#' Returns `dest`.
sync_onedrive_file <- function(src, dest) {
  od <- get_onedrive_session()
  key <- onedrive_manifest_key(onedrive_manifest(src, od))
  if (od_is_current(src, dest, key)) {
    od_log("OneDrive unchanged, skipping download: ", src)
    return(dest)
  }
  od_log("OneDrive downloading: ", src)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  od$download_file(src = src, dest = dest, overwrite = TRUE)
  od_write_stamp(src, dest, key)
  dest
}

#' Download a folder (recursively) from OneDrive to `dest`, skipping the
#' download when the remote manifest hash matches the last download and `dest`
#' still exists. Returns `dest`.
sync_onedrive_folder <- function(src, dest) {
  od <- get_onedrive_session()
  key <- onedrive_manifest_key(onedrive_manifest(src, od))
  if (od_is_current(src, dest, key)) {
    od_log("OneDrive unchanged, skipping download: ", src)
    return(dest)
  }
  od_log("OneDrive downloading: ", src)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  od$download_folder(src = src, dest = dest, overwrite = TRUE,
                      recursive = TRUE, parallel = FALSE)
  od_write_stamp(src, dest, key)
  dest
}

#' Upload a single local file to OneDrive at `dest`. Used to push up
#' expensive-to-compute outputs (e.g. CMIP6 processing) so CI can download
#' already-computed results instead of recomputing them.
upload_onedrive_file <- function(src, dest) {
  od <- get_onedrive_session()
  od$upload_file(src, dest = dest)
  invisible(dest)
}

#' Upload a local folder (recursively) to OneDrive at `dest`. Used to push
#' up expensive-to-compute outputs (e.g. CMIP6 processing) so CI can
#' download already-computed results instead of recomputing them.
upload_onedrive_folder <- function(src, dest) {
  od <- get_onedrive_session()
  files <- list.files(src, full.names = TRUE, recursive = TRUE)
  rel_paths <- substring(files, nchar(src) + 2)
  for (i in seq_along(files)) {
    message("Uploading file: ", rel_paths[i])
    od$upload_file(files[i], dest = paste0(dest, "/", rel_paths[i]))
  }
  invisible(dest)
}

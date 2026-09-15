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

#' Download a single file from OneDrive to `dest`. Returns `dest`.
sync_onedrive_file <- function(src, dest) {
  od <- get_onedrive_session()
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  od$download_file(src = src, dest = dest, overwrite = TRUE)
  dest
}

#' Download a folder (recursively) from OneDrive to `dest`. Returns `dest`.
sync_onedrive_folder <- function(src, dest) {
  od <- get_onedrive_session()
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  od$download_folder(src = src, dest = dest, overwrite = TRUE,
                      recursive = TRUE, parallel = FALSE)
  dest
}

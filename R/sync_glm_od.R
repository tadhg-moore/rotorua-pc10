# Decrypt and restore cache file
bundle    <- openssl::base64_decode(Sys.getenv("ONEDRIVE_TOKEN_ENCRYPTED"))
iv        <- bundle[1:16]
encrypted <- bundle[17:length(bundle)]
key       <- openssl::sha256(charToRaw(Sys.getenv("ONEDRIVE_TOKEN_PASSWORD")))
token_raw <- openssl::aes_cbc_decrypt(encrypted, key = key, iv = iv)

# Write to AzureAuth cache directory
cache_dir  <- AzureAuth::AzureR_dir()
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
writeBin(token_raw, file.path(cache_dir, Sys.getenv("ONEDRIVE_TOKEN_HASH")))

# Load normally
token <- AzureAuth::load_azure_token(hash = Sys.getenv("ONEDRIVE_TOKEN_HASH"))
od    <- Microsoft365R::get_business_onedrive(token = token)

od$list_files(path = "rotorua-pc10/LID11133_rotorua")



src_files <- list.files("LID11133_rotorua/", full.names = TRUE, 
                        recursive = TRUE)
src_files <- src_files[!grepl("/output/|fort|dy_cd|gotm_wet", src_files)]
f <- src_files[1]
for (f in src_files) {
  message("Uploading file: ", f)
  od$upload_file(f, dest = paste0("rotorua-pc10/", f))
}
# od$upload_file(src_files, dest = "rotorua-pc10/LID11133_rotorua/glm_aed/")

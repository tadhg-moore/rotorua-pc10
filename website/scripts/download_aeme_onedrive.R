AEME::install_glm_aed()

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

od$download_file(src = "rotorua-pc10/LID11133_rotorua/aeme.rds", 
                 dest = "../LID11133_rotorua/aeme_download.rds",
                  overwrite = TRUE)

od$download_folder(src = "rotorua-pc10/LakeRotorua", 
                   dest = "LakeRotorua",
                   overwrite = TRUE, recursive = TRUE, parallel = TRUE)

od$download_folder(src = "rotorua-pc10/bin", 
                   dest = "bin",
                   overwrite = TRUE, recursive = TRUE, parallel = TRUE)
od$download_folder(src = "rotorua-pc10/R", 
                   dest = "R",
                   overwrite = TRUE, recursive = TRUE, parallel = TRUE)



aeme <- readRDS("../LID11133_rotorua/aeme_download.rds")

# AEME::plot_output_base(aeme, var_sim = c("temp", "oxy", "tchla"))


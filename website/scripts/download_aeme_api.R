chk <- aemetools::check_api_status()
if (chk) {
  aeme <- aemetools::get_aeme(id = "LID11133")
  saveRDS(aeme, "../LID11133_rotorua/aeme_download.rds")
  
  aeme <- AEME::build_aeme(aeme = aeme, path = "../") |> 
    AEME::run_aeme()
}

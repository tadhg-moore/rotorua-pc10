aeme <- aemetools::get_aeme(id = "LID11133")

aeme <- AEME::build_aeme(aeme = aeme, path = "../") |> 
  AEME::run_aeme()

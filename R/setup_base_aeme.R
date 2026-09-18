setup_base_aeme <- function(path = tempdir(), model = "glm_aed") {
  
  options(AEME.inform = FALSE)
  on.exit(options(AEME.inform = TRUE), add = TRUE)
  lake_dir  <- "LakeRotorua"
  aeme <- AEME::glm_config_to_aeme(nml_file = file.path(lake_dir,
                                                        "glm4_original.nml"))
  
  tgt_vars <- c("HYD_temp", "CHM_salt", "HYD_dens", "RAD_extc", "OXY_oxy", "CAR_dic", "CAR_pH",
                "PHS_frp", "ALU_ala", "ALU_alp", "ALU_uptake_w", "ALU_fpH",
                "ALU_ben_ala", "ALU_ben_alp", "ALU_uptake_s")
  
  model_controls <- AEME::get_model_controls(use_bgc = TRUE)
  model_controls <- AEME::set_vars_sim(model_controls, tgt_vars)
  
  aeme <- aeme |> 
    AEME::build_aeme(use_bgc = TRUE, path = path, ext_elev = 3)
  aeme_path <- AEME::get_lake_dir(aeme)
  
  aed_old_file <- file.path(lake_dir, "aed", "aed_original.nml")
  aed_file <- file.path(aeme_path, "glm_aed", "aed", "aed.nml")
  
  aed_old <- AEME::read_nml(aed_old_file)
  aed <- AEME::read_nml(aed_file)
  aed$aed_models$models <- c(aed$aed_models$models, "aed_alum")
  aed$aed_alum <- aed_old$aed_alum
  AEME::write_nml(aed, aed_file)
  
  aeme <- aeme |> 
    AEME::run_aeme()
}

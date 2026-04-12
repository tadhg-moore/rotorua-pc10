rotorua_glm_parameters <- function() {
  sed_params <- AEME::glm_sed_params(zone_heights = c(5, 25, 47))
  tibble::tribble(
    ~model, ~file, ~name, ~value, ~min, ~max, ~group, ~index,
  )
}



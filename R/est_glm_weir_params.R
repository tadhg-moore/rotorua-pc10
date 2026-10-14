est_glm_weir_params <- function(ohau_flow_level, crest_width_m = 26) {
  d <- subset(
    ohau_flow_level,
    is.finite(level) & is.finite(flow) & flow >= 0
  )
  
  glm_coefficient <- (2 / 3) * sqrt(2 * 9.81)

  fit_at_crest <- function(crest_m) {
    base_flow <- glm_coefficient * crest_width_m *
      pmax(d$level - crest_m, 0)^1.5
    
    crest_factor <- sum(base_flow * d$flow) / sum(base_flow^2)
    
    c(
      sse = sum((d$flow - crest_factor * base_flow)^2),
      crest_factor = crest_factor
    )
  }
  
  fit <- optimize(
    function(crest_m) fit_at_crest(crest_m)[["sse"]],
    interval = c(278.166, 279.316)
  )
  
  crest_elev <- fit$minimum
  crest_factor <- fit_at_crest(crest_elev)[["crest_factor"]]
  
  list(crest_elev = crest_elev, crest_factor = crest_factor)
}

format_weir_params <- function(crest_elev, crest_factor, crest_width) {
  weir_params <- tibble::tribble(
    ~name, ~value,
    "morphometry/crest_elev", crest_elev,
    "outflow/crest_width", crest_width,
    "outflow/crest_factor", crest_factor,
    "outflow/num_outlet", 0
  ) |> 
    dplyr::mutate(
      model = "glm_aed",
      file = "glm4.nml",
      min = dplyr::case_when(
        name == "morphometry/crest_elev" ~ crest_elev - 0.2,
        name == "outflow/crest_factor" ~ crest_factor - 0.1,
        TRUE ~ value
      ),
      max = dplyr::case_when(
        name == "morphometry/crest_elev" ~ crest_elev + 0.2,
        name == "outflow/crest_factor" ~ crest_factor + 0.2,
        TRUE ~ value
      )
    ) |> 
    dplyr::select(model, file, name, value, min, max)
  return(weir_params)
}

glm_weir_outflow <- function(level, crest_factor, crest_height, crest_width) {
  (2 / 3) * crest_factor * sqrt(2 * 9.81) * crest_width * 
    pmax(level - crest_height, 0)^1.5
}

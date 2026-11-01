set_lake_inflows <- function(
    aeme,
    lake_routing,
    lake_metadata,
    scenario,
    spinup_mapping,
    outflow_inflow = NULL,
    aeme_inf = NULL,
    inflow_dir = here::here("data", "processed")
) {
  
  lke <- AEME::lake(aeme)
  lake_area_ha <- lke$area / 10000
  lakename <- lake_metadata |> 
    dplyr::filter(lernzmp_id == lke$id) |>
    dplyr::pull(lake)
  
  
  # ---- Select inflows for lake ----
  lake_inflows <- lake_routing |>
    dplyr::filter(LakeName == lakename)
  
  sel_inflows <- lake_inflows |>
    dplyr::filter(`Downstream SWSID` < 100)
  
  sel_inflow_precip <- lake_inflows |>
    dplyr::filter(contains_lake)
  
  if (nrow(sel_inflow_precip) > 1) {
    stop(
      "Expected exactly one inflow that contains the lake for lake: ",
      lakename, " but found ", nrow(sel_inflow_precip)
    )
  }
  
  # ---- Load inflow data ----
  inflow_file <- file.path(
    inflow_dir,
    paste0("lake_flows_hourly_", scenario, ".csv")
  )
  
  lake_flows_hourly <- readr::read_csv(
    inflow_file,
    show_col_types = FALSE
  )
  
  # ---- Process inflow catchments ----
  if (nrow(sel_inflows) > 0) {
    
    inflows <- lapply(sel_inflows$SWSID, function(swsid) {
      
      inf <- lake_flows_hourly |>
        dplyr::filter(watershed_id == swsid)
      
      if (nrow(inf) == 0) return(NULL)
      
      flow_var <- "ro"
      
      inflow <- lspc_to_aeme(inf, flow_var = flow_var)
      inflow <- apply_spinup_mapping(
        df = inflow,
        spinup_mapping = spinup_mapping
      )
      
      inflow
    })
    
    names(inflows) <- paste0("swsid_", sel_inflows$SWSID)
    inflows <- inflows[!vapply(inflows, is.null, logical(1))]
    
    # ---- Precipitation inflow ----
    precip_area <- sel_inflow_precip |>
      dplyr::pull(drainage_area_ha)
    
    precip_df <- lake_flows_hourly |>
      dplyr::filter(watershed_id %in% sel_inflow_precip$SWSID) |>
      dplyr::mutate(Date = as.Date(datetime)) |>
      dplyr::group_by(Date) |>
      dplyr::summarise(
        HYD_flow = sum(prec, na.rm = TRUE),
        airtemp  = mean(temp),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        HYD_flow = HYD_flow * (lake_area_ha / precip_area),
        HYD_temp = zoo::rollmeanr(airtemp, 3, fill = NA)
      )
    
    precip_inflow <- precip_df |>
      dplyr::mutate(
        HYD_temp = ifelse(is.na(HYD_temp), airtemp, HYD_temp),
        CHM_salt = 0,
        CHM_oxy  = 10,
        NIT_tn   = 0,
        NIT_nit  = 0,
        NIT_amm  = 0,
        NIT_din  = 0,
        PHS_tp   = 0,
        PHS_frp  = 0,
        NCS_tss  = 0,
        NCS_ss1  = 0,
        NIT_don  = 0,
        NIT_pon  = 0,
        PHS_dop  = 0,
        PHS_pop  = 0,
        PHS_pip  = 0,
        CAR_doc  = 0,
        CAR_poc  = 0,
        SIL_rsi  = 0
      ) |>
      dplyr::select(-airtemp)
    
    precip_inflow <- apply_spinup_mapping(
      df = precip_inflow,
      spinup_mapping = spinup_mapping
    )
    
    inflows[["precip"]] <- precip_inflow
    
    # ---- Optional outflow-as-inflow ----
    if (!is.null(outflow_inflow)) {
      inflows[["outflow_inflow"]] <- outflow_inflow
    }
    
    aeme <- AEME::set_inflows(aeme, inflows = inflows)
    
  } else {
    
    # ---- Remove non-precip inflows ----
    if (!is.null(aeme_inf)) {
      for (nm in names(aeme_inf)) {
        if (nm == "precip") next
        aeme <- AEME::remove_inflow(aeme, nm)
      }
    }
  }
  
  return(aeme)
}

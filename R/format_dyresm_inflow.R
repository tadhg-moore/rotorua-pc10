format_dyresm_inflow <- function(rotorua_inflow_file, rotorua_inflow_id_file, 
                                 inflow_key_file) {
  inf <- readr::read_csv(rotorua_inflow_file) |>
    dplyr::mutate(
      Date = lubridate::as_date(Date, format = "%d/%m/%Y"),
      dplyr::across(where(is.numeric), \(x) ifelse(is.na(x), 0, x))
    )
  
  inf_id <- readr::read_csv(rotorua_inflow_id_file) |> 
    dplyr::rename(InfNum = x, Inflow = `Group.1`)
  
  inflow_key <- readr::read_csv(inflow_key_file)
  
  inf_nums <- inf_id |> 
    dplyr::distinct(InfNum) |> 
    dplyr::pull(InfNum)
  i <- 1
  list_inf <- lapply(inf_nums, \(i) {
    dat <- inf |> 
      dplyr::filter(InfNum == i) |> 
      dplyr::select(-InfNum) |>
      dplyr::rename(
        PH = pH
      ) |> 
      dplyr::mutate(
        PH = dplyr::case_when(
          PH < 5 ~ median(PH),
          .default = PH
        )
      )
    inflow_name <- inf_id |> 
      dplyr::filter(InfNum == i) |> 
      dplyr::pull(Inflow) |> 
      tolower()
    
    # df <- list_inf[[i]] |> 
    #   dplyr::select(-InfNum)
    
    # Rename to match var column in inflow_key
    col_names <- colnames(dat)
    new_col_names <- sapply(col_names, function(c) {
      new_name <- inflow_key |>
        dplyr::filter(grepl(c, dyresm, ignore.case = TRUE)) |>
        dplyr::pull(var_aeme)
      if (length(new_name) == 0) {
        warning(paste("No match found for column:", c))
        return(c)
      }
      return(new_name[1])
    })
    colnames(dat) <- new_col_names
    
    # summary(dat)
    return(dat)
  })
  
  # Write to csv file
  dir.create("data/processed/rotorua_data_sh/inflows", showWarnings = FALSE,
             recursive = TRUE)
  for (i in seq_along(list_inf)) {
    inflow_name <- inf_id |> 
      dplyr::filter(InfNum == inf_nums[i]) |> 
      dplyr::pull(Inflow) |> 
      tolower()
    
    df <- list_inf[[i]] 
    print(summary(df))
    
    out_file <- paste0("data/processed/rotorua_data_sh/inflows/inflow_", inflow_name, 
                       ".csv")
    readr::write_csv(df, out_file)
  }
  names(list_inf) <- inf_id$Inflow[match(inf_nums, inf_id$InfNum)]
  return(list_inf)
}

convert_dyresm_to_aeme <- function(df) {
  
  data("key_naming", package = "AEME")
  inf_nums <- df |> 
    dplyr::distinct(InfNum) |> 
    dplyr::pull(InfNum)
  list_inf <- lapply(inf_nums, \(i) {
    dat <- df |> 
      dplyr::filter(InfNum == i) |> 
      dplyr::select(-InfNum) |> 
      dplyr::rename(
        PH = pH
      ) |> 
      dplyr::mutate(
        PH = dplyr::case_when(
          PH < 5 ~ median(PH),
          .default = PH
        )
      )
    cnams <- AEME:::rename_modelvars(input = colnames(dat), type_input = "dy_cd",
                                            type_output = "glm_aed")
    colnames(dat) <- cnams
    
    dat <- dat |> 
      dplyr::rename(
        Date = time,
        HYD_flow = flow
      ) |> 
      dplyr::mutate(HYD_flow = HYD_flow / 86400)
    # for (c in colnames(dat)[-1]) {
    #   mult <- key_naming |>
    #     dplyr::filter(glm_aed == c) |>
    #     dplyr::pull(conversion_aed)
    #   dat[, c] <- round(dat[, c] / mult, 5)
    # }
    dat <- dat |> 
      dplyr::mutate(
        PHY_cyano = 0.1,
        PHY_green = 0.1,
        PHY_diatom = 0.1
      )
    summary(dat)
    return(dat)
  })
  names(list_inf) <- paste0("INF", inf_nums)

  
  return(list_inf)
}

get_lake_wqprofiler <- function(lake = "rotorua", type = "pro",
                                api_url = "https://api.limnotrack.com", 
                                api_key = Sys.getenv("LERNZMP_KEY")) {
  
  res <- aemetools::api_request(api_url = api_url, api_key = api_key,
                                endpoint = "get_lake_wqprofiler", 
                                query = list(lake = lake, type = type))
  
  if (httr2::resp_status(res) != 200) {
    cli::cli_abort("API request failed with status: ", httr2::resp_status(res))
  }
  
  df <- res |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON()
  
  return(df)
}

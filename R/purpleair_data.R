
purpleair_api_site = "https://api.purpleair.com/v1/"
purpleair_api_channels = c("sensors", "groups", "organization", "keys")

purpleair_api_calls = list(
  keys = list(
    "Check API Key" = "keys"
  ),
  organization = list(
    "Get Organization Data" = "organization"
  ),
  sensors = list(
    "Get Sensor Data" = "sensors/:sensor_index",
    "Get Sensor History" = "sensors/:sensor_index/history",
    "Get Sensors Data" = "sensors"
  ),
  groups = list(
    "Create Group" = "groups",
    "Create Member" = "groups/:group_id/members",
    "Delete Group" = "groups/:group_id",
    "Delete Member" = "groups/:group_id/members/:member_id",
    "Get Group Detail" = "groups/:group_id",
    "Get Groups List" = "groups",
    "Get Member Data" = "groups/:group_id/members/:member_id", 
    "Get Member History" = "groups/:group_id/members/:member_id/history/csv",
    "Get Members Data" = "groups/:group_id/members"
  )
)

get_purpleair_api_call = function(channel, parameters){
  p_names = names(parameters)
  if(channel %in% c("keys", "organization")){
    call = names(purpleair_api_calls[[channel]])[1]
  }else if(channel == "sensors"){
    call = dplyr::case_when(
      # sensor_index not provided
      !"sensor_index" %in% p_names ~ "Get Sensors Data",
      # sensor_index and a start/end date provided
      "start_timestamp" %in% p_names | 
           "end_timestamp" %in% p_names ~ "Get Sensor History",
      # sensor_index provided but not dates
      "sensor_index" %in% p_names ~ "Get Sensor Data")
  }else if(channel == "groups"){
    if(!is.null(write_key)){ # If write key provided
      call = dplyr::case_when(
        # group_id not provided
        !"group_id" %in% p_names ~ "Create Group",
        # group id provided but not other ids
        !any(c("sensor_index", "sensor_id", "member_id") %in% p_names) ~ "Delete Group",
        # group_id and member_id provided
        "member_id" %in% p_names ~ "Delete Member",
        c("sensor_index", "sensor_id") %in% p_names ~ "Create Member")
    }else{ # If read key provided
      call = dplyr::case_when(
        # group_id not provided
        !"group_id" %in% p_names ~ "Get Groups List",
        # group_id and fields provided 
        "fields" %in% p_names ~ "Get Members Data",
        # group_id provided but not fields nor member_id
        !"member_id" %in% p_names ~ "Get Group Detail",
        # group_id, member_id, and start/end date provided
        any(c("start_timestamp", "end_timestamp") %in% p_names) ~ "Get Member History",
        # group_id and member_id provided but no dates
        "member_id" %in% p_names ~ "Get Member Data")
    }
  }
}

purpleair_api = function(read_key = NULL, write_key = NULL, channel, parameters = NULL, verbose = TRUE){
  # Handle inputs
  if(is.null(read_key) & is.null(write_key))
    stop("Either read_key or write_key must be provided.")
  if(!is.null(read_key) & !is.null(write_key))
    stop("Either read_key or write_key must be provided, not both.")
  if(!channel %in% purpleair_api_channels) 
    stop(paste("channel must be one of:", paste(purpleair_api_channels, collapse = ", ")))
  if(channel %in% c("organization", "sensors", "keys"))
    if(is.null(read_key)) 
      stop(paste(channel, "channel is read only"))
  
  if("fields" %in% names(parameters)){
    parameters$fields = paste0(parameters$fields, collapse = ",")
  }
  if("start_timestamp" %in% names(parameters)){
    parameters$start_timestamp = lubridate::as_datetime(parameters$start_timestamp) |>
      as.numeric()
  }
  if("end_timestamp" %in% names(parameters)){
    parameters$end_timestamp = lubridate::as_datetime(parameters$end_timestamp) |>
      as.numeric()
  }
  
  call_msg = function(channel, call){
    url = paste0(
      "https://api.purpleair.com/#api-", channel, "-", 
      stringr::str_to_lower(call) |> 
        stringr::str_replace_all(" ", "-") |> 
        paste(collapse = "-"))
    paste0(
      "Calling `", call, "`", "(See: ", url, ")")
  }  

  call = get_purpleair_api_call(channel, parameters)

  if(verbose) message(call_msg(channel, call))

  channel_suffix = purpleair_api_calls[[channel]][[call]]
  if(stringr::str_detect(channel_suffix, ":sensor_index")){
    channel_suffix = channel_suffix |>
      stringr::str_replace(":sensor_index", as.character(parameters$sensor_index[1]))
  }
  if(stringr::str_detect(channel_suffix, ":group_id")){
    channel_suffix = channel_suffix |>
      stringr::str_replace(":group_id", parameters$group_id)
  }
  if(stringr::str_detect(channel_suffix, ":member_id")){
    channel_suffix = channel_suffix |>
      stringr::str_replace(":member_id", parameters$member_id)
  }
  
  results = httr::GET(
    paste0(purpleair_api_site, channel_suffix), 
    httr::add_headers("X-API-Key" = read_key), query = parameters
  ) |>
    httr::content()

  if("data" %in% names(results)){
    results$data = results$data |>
      lapply(\(d) as.data.frame(t(unlist(setNames(d, results$fields))))) |>
      dplyr::bind_rows()  
    results$data = results$data[, 1:length(results$fields)] 
    
    if(! "time_stamp" %in% names(results$data)) results$data$time_stamp = results$data_time_stamp
    if(! "sensor_index" %in% names(results$data)) results$data$sensor_index = results$sensor_index
    results$data = results$data |>
      dplyr::mutate(time_stamp = lubridate::as_datetime(.data$time_stamp, tz = "UTC")) |>
      dplyr::relocate("sensor_index", .before = 1) |>
      dplyr::relocate("time_stamp", .before = "sensor_index")
    results = results$data |> dplyr::arrange(.data$time_stamp)
  }
  if("sensor" %in% names(results)){
    results$data = as.data.frame(results$sensor) |>
      dplyr::mutate(time_stamp = lubridate::as_datetime(results$data_time_stamp, tz = "UTC")) |>
      dplyr::relocate("time_stamp", .before = "sensor_index")
    results = results$data  |> dplyr::arrange(.data$time_stamp)
  }

  return(results)
}

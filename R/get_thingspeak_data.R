#' Get data from a ThingSpeak channel
#'
#' @param channel_id ThingSpeak channel ID
#' @param try_to_include_units If TRUE, the function will try to include units in the response by looking for "\[unit\]" in the column names.
#' @param read_params (Optional) parameters to include in the request created using [ThingSpeakReadParams()].
#'   See \href{https://www.mathworks.com/help/thingspeak/readdata.html}{the thingspeak documentation} for valid parameters or [ThingSpeakReadParams()].
#' @param ... (Optional) Named parameters to include in the request if read_params is NULL.
#'   See \href{https://www.mathworks.com/help/thingspeak/readdata.html}{the thingspeak documentation} for valid parameters or [ThingSpeakReadParams()].
#' @return A list with channel metadata and a tibble containing the data from the ThingSpeak channel.
#' @examples
#' \dontrun{
#' data <- get_thingspeak_data(channel_id = "123456")
#' }
#' @export
get_thingspeak_data <- function(
  channel_id,
  try_to_include_units = TRUE,
  read_params = NULL,
  ...
) {
  stopifnot(
    "`read_params` must be created using `ThingSpeakReadParams()`" = "airquality::ThingSpeakReadParams" %in%
      class(read_params) |
      is.null(read_params)
  )

  # Build basic channel url
  template <- "https://api.thingspeak.com/channels/%s/feeds.json"
  url <- template |> sprintf(channel_id)

  # Add parameters if any
  if (is.null(read_params)) {
    valid_fields <- names(ThingSpeakReadParams@properties)
    params <- rlang::dots_list(...)
    params <- params[names(params) %in% valid_fields]
    read_params <- do.call(ThingSpeakReadParams, params)
  }
  params <- read_params |>
    S7::props()
  params <- params[sapply(params, length) > 0]
  url <- url |>
    insert_thingspeak_params(params = params)

  # Make GET request and parse
  result <- url |>
    httr::GET() |>
    httr::content(as = "text", encoding = "UTF-8") |>
    rjson::fromJSON()

  # Extract field names if present
  field_pattern <- "^field[0-9]+$"
  fields <- result$channel[
    field_pattern |> grep(x = names(result$channel))
  ] |>
    unlist()
  if (length(fields) > 0) {
    field_order <- names(fields) |>
      stringr::str_remove(pattern = "field") |>
      as.numeric() |>
      order()
    fields <- fields[field_order]
  }

  # Extract any other metadata
  meta <- result$channel[
    field_pattern |> grep(x = names(result$channel), invert = TRUE)
  ] |>
    lapply(\(x) x |> type.convert(as.is = TRUE))
  if ("created_at" %in% names(meta)) {
    meta$created_at <- lubridate::as_datetime(meta$created_at)
  }
  if ("updated_at" %in% names(meta)) {
    meta$updated_at <- lubridate::as_datetime(meta$updated_at)
  }

  # Extract data and format values
  data <- result$feeds |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      created_at = lubridate::as_datetime(.data$created_at),
      dplyr::across(-.data$created_at, \(x) x |> type.convert(as.is = TRUE))
    )

  # Rename data columns if fields are present
  # and try to include units if desired
  if (length(fields) == (ncol(data) - 2)) {
    names(data)[-(1:2)] <- fields
    if (try_to_include_units) {
      data <- data |> convert_thingspeak_units()
    }
  }

  meta |> c(list(params = params, data = data))
}

get_thingspeak_public_channels <- function(page = 1) {
  # Build url to desired page of channels
  url <- "https://thingspeak.mathworks.com/channels/public?page=%s" |>
    sprintf(page)

  # Scrape text content
  page_lines <- url |>
    rvest::read_html() |>
    rvest::html_text() |>
    stringr::str_split_1("\n")

  # Extract channel IDs
  id_entries <- page_lines |>
    stringr::str_which("^ *Channel ID:") +
    1L
  page_lines[id_entries] |>
    as.numeric()
}

insert_thingspeak_params <- function(url, params) {
  if (length(params) == 0) {
    return(url)
  }

  if (length(params) != 0) {
    params[sapply(params, is.logical)] <- params[sapply(params, is.logical)] |>
      lapply(tolower)
    params <- names(params) |>
      paste0("=", params) |>
      paste(collapse = "&")
    url <- url |>
      paste0("?", params)
  }
}

convert_thingspeak_units <- function(data) {
  unit_pattern <- " \\[(.*)\\]$"
  has_units <- names(data)[-(1:2)] |> stringr::str_detect(unit_pattern)
  if (!any(has_units)) {
    return(data)
  }
  unit_cols <- names(data)[-(1:2)][has_units]
  data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(unit_cols),
        \(x) {
          unit <- dplyr::cur_column() |>
            stringr::str_extract(unit_pattern, group = 1) |>
            stringr::str_replace(".*%.*", "%") # remove all but percent if percent detected

          is_valid_unit <- x[1] |>
            as.numeric() |>
            units::set_units(unit, mode = "standard") |>
            handyr::on_error(.return = FALSE)
          if (is_valid_unit) {
            x |>
              as.numeric() |>
              units::set_units(unit, mode = "standard")
          } else {
            x
          }
        }
      )
    ) |>
    dplyr::rename_with(
      .cols = dplyr::where(\(x) "units" %in% class(x)),
      .fn = \(x) stringr::str_remove(x, unit_pattern)
    )
}

# S7 validator for thingspeak "timescale"/"sum"/"average"/"median"
class_ts_timescale <- S7::class_integer |>
  S7::new_union(S7::class_character) |>
  S7::new_property(validator = \(value) {
    allowed <- c(10, 15, 20, 30, 60, 240, 720, 1440, "daily")
    if (length(value) > 1) {
      "must be a single value if provided"
    }
    if (length(value)) {
      if (!value %in% allowed) {
        paste0("must be one of ", paste(allowed, collapse = ", "))
      }
    }
  })

# S7 validator for thingspeak date start/end
class_ts_datestring <- S7::class_character |>
  S7::new_property(validator = \(value) {
    if (length(value)) {
      if (
        !stringr::str_detect(
          value,
          "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$"
        )
      ) {
        "must be the format 'YYYY-MM-DD HH:MM:SS'"
      }
    }
  })

#' @export
ThingSpeakReadParams <- S7::new_class(
  "ThingSpeakReadParams",
  properties = list(
    api_key = S7::class_character,
    results = S7::class_integer |>
      S7::new_property(validator = \(value) {
        if (length(value)) {
          if (value > 8000) {
            "must be 8000 or less"
          }
        }
      }),
    days = S7::class_integer |>
      S7::new_property(default = 1L),
    minutes = S7::class_integer |>
      S7::new_property(default = 1440L),
    start = class_ts_datestring,
    end = class_ts_datestring,
    timezone = S7::class_character,
    offset = S7::class_integer,
    fields = S7::class_character,
    status = S7::class_logical,
    metadata = S7::class_logical |>
      S7::new_property(default = TRUE),
    location = S7::class_logical |>
      S7::new_property(default = TRUE),
    min = S7::class_numeric,
    max = S7::class_numeric,
    round = S7::class_integer,
    timescale = class_ts_timescale,
    sum = class_ts_timescale,
    average = class_ts_timescale,
    median = class_ts_timescale
  )
)

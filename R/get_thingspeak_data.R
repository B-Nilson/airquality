#' Get data from a ThingSpeak channel
#'
#' @param channel_id ThingSpeak channel ID
#' @param read_params (Optional) parameters to include in the request created using [ThingSpeakReadParams()].
#'   See \href{https://www.mathworks.com/help/thingspeak/readdata.html}{the thingspeak documentation} for valid parameters or [ThingSpeakReadParams()].
#'   Defaults to NULL, which uses `...` to build `read_params` instead.
#' @param ... (Optional) Named parameters to include in the request if read_params is NULL.
#'   See \href{https://www.mathworks.com/help/thingspeak/readdata.html}{the thingspeak documentation} for valid parameters or [ThingSpeakReadParams()].
#'   Will be used to build `read_params` using [ThingSpeakReadParams()].
#'   Default (nothing passed) is equivalent to `read_params = ThingSpeakReadParams()`.
#' @param try_to_include_units If TRUE, the function will try to include units in the response by looking for "\[unit\]" in the column names.
#' @return A list with channel metadata and a tibble containing the data from the ThingSpeak channel.
#' @examples
#' \dontrun{
#' data <- get_thingspeak_data(channel_id = 123456)
#' }
#' @export
get_thingspeak_data <- function(
  channel_id,
  read_params = NULL,
  ...,
  try_to_include_units = TRUE
) {
  stopifnot(
    is.numeric(channel_id) | !is.na(as.integer(channel_id)),
    length(channel_id) == 1
  )
  stopifnot(is.logical(try_to_include_units), length(try_to_include_units) == 1)
  stopifnot(
    "`read_params` must be created using `ThingSpeakReadParams()`" = "airquality::ThingSpeakReadParams" %in%
      class(read_params) |
      is.null(read_params)
  )

  # Build basic channel url
  template <- "https://api.thingspeak.com/channels/%s/feeds.json"
  url <- template |> sprintf(as.integer(channel_id))

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
    lapply(\(x) x |> utils::type.convert(as.is = TRUE))
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
      dplyr::across(-dplyr::any_of("created_at"), \(x) {
        x |> utils::type.convert(as.is = TRUE)
      })
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

get_thingspeak_public_channels <- function(pages = 1, quiet = FALSE) {
  stopifnot(is.numeric(pages) | !is.na(as.integer(pages)), length(pages) > 0)
  stopifnot(is.logical(quiet), length(quiet) == 1)

  # Build url to desired page(s) of channels
  urls <- "https://thingspeak.mathworks.com/channels/public?page=%s" |>
    sprintf(as.integer(pages))

  # Scrape text content
  page_lines <- urls |>
    handyr::for_each(
      .show_progress = !quiet,
      \(url) {
        url |>
          rvest::read_html() |>
          rvest::html_text()
      }
    ) |>
    stringr::str_split("\n") |>
    unlist()

  # Extract channel IDs and titles
  id_entries <- page_lines |>
    stringr::str_which("^ *Channel ID:")
  titles <- page_lines[id_entries - 5L] |>
    stringr::str_trim()
  page_lines[id_entries + 1L] |>
    as.numeric() |>
    stats::setNames(object = titles)
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

#' An S7 class to represent parameters for the ThingSpeak API.
#' @description
#'   Provides a structured way to specify parameters for the ThingSpeak API.
#'   Intended to be used with [get_thingspeak_data()] for building queries.
#'
#'   Note: The `results` parameter has the highest precedence.
#'   Using `results` with the parameters `min`, `max`, `timescale`, `sum`, `average`, or `median` can cause less records than `results` to be returned.
#'   The results parameter determines the maximum number of entries to be used for a query, up to 8000.
#'   For example, a request to a channel with one entry per minute with the parameters `results = 120` and `sum = 60` returns only two records, and not 120.
#'
#'   See \href{https://www.mathworks.com/help/thingspeak/readdata.html}{the thingspeak documentation} for more information.
#' @slot api_key Required for private channels. Specify the Read API Key found on the API Keys tab of the channel view. (string)
#' @slot results Optional. Number of entries to retrieve. Maximum is 8,000. (integer)
#' @slot days Optional. Number of 24-hour periods before now to include. Default is 1. (integer)
#' @slot minutes Optional. Number of 60-second periods before now to include. Default is 1440. (integer)
#' @slot start Optional. Start date in format YYYY-MM-DD HH:NN:SS. (datetime)
#' @slot end Optional. End date in format YYYY-MM-DD HH:NN:SS. (datetime)
#' @slot timezone Optional. Identifier from Time Zones Reference. (string)
#' @slot offset Optional. Timezone offset for displaying results. Use `timezone` for greater accuracy. (integer)
#' @slot status Optional. Include status updates by setting `status = TRUE`. (logical)
#' @slot metadata Optional. Include channel metadata by setting `metadata = TRUE`. (logical)
#' @slot location Optional. Include latitude, longitude, and elevation by setting `location = TRUE`. (logical)
#' @slot min Optional. Minimum value to include. (numeric)
#' @slot max Optional. Maximum value to include. (numeric)
#' @slot round Optional. Round values to this many decimal places. (integer)
#' @slot timescale Optional. Get first value in this many minutes. Valid values: 10, 15, 20, 30, 60, 240, 720, 1440, "daily". (integer or string)
#' @slot sum Optional. Get sum over this many minutes. Valid values: 10, 15, 20, 30, 60, 240, 720, 1440, "daily". (integer or string)
#' @slot average Optional. Get average over this many minutes. NaN values are treated as 0. Valid values: 10, 15, 20, 30, 60, 240, 720, 1440, "daily". (integer or string)
#' @slot median Optional. Get median over this many minutes. Valid values: 10, 15, 20, 30, 60, 240, 720, 1440, "daily". (integer or string)
#'
#' @import S7
#' @export
ThingSpeakReadParams <- new_class(
  "ThingSpeakReadParams",
  properties = list(
    api_key = class_character,
    results = class_integer |>
      new_property(validator = \(value) {
        if (length(value)) {
          if (value > 8000) {
            "must be 8000 or less"
          }
        }
      }),
    days = class_integer |>
      new_property(default = 1L),
    minutes = class_integer |>
      new_property(default = 1440L),
    start = class_ts_datestring,
    end = class_ts_datestring,
    timezone = class_character,
    offset = class_integer,
    fields = class_character,
    status = class_logical,
    metadata = class_logical |>
      new_property(default = TRUE),
    location = class_logical |>
      new_property(default = TRUE),
    min = class_numeric,
    max = class_numeric,
    round = class_integer,
    timescale = class_ts_timescale,
    sum = class_ts_timescale,
    average = class_ts_timescale,
    median = class_ts_timescale
  )
)

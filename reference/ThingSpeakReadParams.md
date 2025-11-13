# An S7 class to represent parameters for the ThingSpeak API.

Provides a structured way to specify parameters for the ThingSpeak API.
Intended to be used with \[get_thingspeak_data()\] for building queries.

Note: The \`results\` parameter has the highest precedence. Using
\`results\` with the parameters \`min\`, \`max\`, \`timescale\`,
\`sum\`, \`average\`, or \`median\` can cause less records than
\`results\` to be returned. The results parameter determines the maximum
number of entries to be used for a query, up to 8000. For example, a
request to a channel with one entry per minute with the parameters
\`results = 120\` and \`sum = 60\` returns only two records, and not
120.

See [the thingspeak
documentation](https://www.mathworks.com/help/thingspeak/readdata.html)
for more information.

## Usage

``` r
ThingSpeakReadParams(
  api_key = character(0),
  results = integer(0),
  days = 1L,
  minutes = 1440L,
  start = character(0),
  end = character(0),
  timezone = character(0),
  offset = integer(0),
  fields = character(0),
  status = logical(0),
  metadata = TRUE,
  location = TRUE,
  min = integer(0),
  max = integer(0),
  round = integer(0),
  timescale = integer(0),
  sum = integer(0),
  average = integer(0),
  median = integer(0)
)
```

## Slots

- `api_key`:

  Required for private channels. Specify the Read API Key found on the
  API Keys tab of the channel view. (string)

- `results`:

  Optional. Number of entries to retrieve. Maximum is 8,000. (integer)

- `days`:

  Optional. Number of 24-hour periods before now to include. Default
  is 1. (integer)

- `minutes`:

  Optional. Number of 60-second periods before now to include. Default
  is 1440. (integer)

- `start`:

  Optional. Start date in format YYYY-MM-DD HH:NN:SS. (datetime)

- `end`:

  Optional. End date in format YYYY-MM-DD HH:NN:SS. (datetime)

- `timezone`:

  Optional. Identifier from Time Zones Reference. (string)

- `offset`:

  Optional. Timezone offset for displaying results. Use \`timezone\` for
  greater accuracy. (integer)

- `status`:

  Optional. Include status updates by setting \`status = TRUE\`.
  (logical)

- `metadata`:

  Optional. Include channel metadata by setting \`metadata = TRUE\`.
  (logical)

- `location`:

  Optional. Include latitude, longitude, and elevation by setting
  \`location = TRUE\`. (logical)

- `min`:

  Optional. Minimum value to include. (numeric)

- `max`:

  Optional. Maximum value to include. (numeric)

- `round`:

  Optional. Round values to this many decimal places. (integer)

- `timescale`:

  Optional. Get first value in this many minutes. Valid values: 10, 15,
  20, 30, 60, 240, 720, 1440, "daily". (integer or string)

- `sum`:

  Optional. Get sum over this many minutes. Valid values: 10, 15, 20,
  30, 60, 240, 720, 1440, "daily". (integer or string)

- `average`:

  Optional. Get average over this many minutes. NaN values are treated
  as 0. Valid values: 10, 15, 20, 30, 60, 240, 720, 1440, "daily".
  (integer or string)

- `median`:

  Optional. Get median over this many minutes. Valid values: 10, 15, 20,
  30, 60, 240, 720, 1440, "daily". (integer or string)

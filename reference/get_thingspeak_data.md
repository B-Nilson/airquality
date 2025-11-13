# Get data from a ThingSpeak channel

Get data from a ThingSpeak channel

## Usage

``` r
get_thingspeak_data(
  channel_id,
  read_params = NULL,
  ...,
  try_to_include_units = TRUE
)
```

## Arguments

- channel_id:

  ThingSpeak channel ID

- read_params:

  (Optional) parameters to include in the request created using
  \[ThingSpeakReadParams()\]. See [the thingspeak
  documentation](https://www.mathworks.com/help/thingspeak/readdata.html)
  for valid parameters or \[ThingSpeakReadParams()\]. Defaults to NULL,
  which uses \`...\` to build \`read_params\` instead.

- ...:

  (Optional) Named parameters to include in the request if read_params
  is NULL. See [the thingspeak
  documentation](https://www.mathworks.com/help/thingspeak/readdata.html)
  for valid parameters or \[ThingSpeakReadParams()\]. Will be used to
  build \`read_params\` using \[ThingSpeakReadParams()\]. Default
  (nothing passed) is equivalent to \`read_params =
  ThingSpeakReadParams()\`.

- try_to_include_units:

  If TRUE, the function will try to include units in the response by
  looking for "\\unit\\" in the column names.

## Value

A list with channel metadata and a tibble containing the data from the
ThingSpeak channel.

## Examples

``` r
if (FALSE) { # \dontrun{
data <- get_thingspeak_data(channel_id = 123456)
} # }
```

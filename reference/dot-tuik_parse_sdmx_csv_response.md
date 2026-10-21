# Parse an SDMX-CSV 2.0 response

Reads all columns as character to preserve SDMX codes with leading
zeros. OBS_VALUE can optionally be converted to numeric when every
non-blank value is safely convertible.

## Usage

``` r
.tuik_parse_sdmx_csv_response(resp, parse_observation = TRUE)
```

## Arguments

- resp:

  An httr2 response containing SDMX-CSV.

- parse_observation:

  Logical. Convert OBS_VALUE to numeric when safe.

## Value

A tibble.

# Retrieve codes for a DSD dimension

Finds the Codelist referenced by a dimension and returns its codes.

## Usage

``` r
tuik_dimension_codes(
  dimensions,
  dimension,
  search = NULL,
  language = c("both", "tr", "en"),
  limit = 50,
  api_key = NULL
)
```

## Arguments

- dimensions:

  Dimension table returned by
  [`tuik_dimensions()`](https://MFatihTuzen.github.io/tuiksdmx/reference/tuik_dimensions.md)
  or a `tuik_flow_profile` object.

- dimension:

  Dimension ID, for example "FREQ" or "COICOP_2018".

- search:

  Optional search term.

- language:

  Search language: "both", "tr", or "en".

- limit:

  Maximum number of rows to return. Use NULL for all rows.

- api_key:

  Optional API key. If NULL, the TUIK_SDMX_API_KEY environment variable
  is used.

## Value

A tibble containing the dimension codes.

## Details

Optionally searches within code values and Turkish/English labels.

## Examples

``` r
if (FALSE) { # \dontrun{
tuik_dimension_codes(
  dimensions,
  "FREQ"
)

tuik_dimension_codes(
  dimensions,
  "COICOP_2018",
  search = "food"
)
} # }
```

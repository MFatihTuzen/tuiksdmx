# Add SDMX code labels to an observation table

Joins dimension code labels from the Codelists referenced by a Dataflow
profile. Original SDMX dimension codes and observations are preserved.
The labels describe Codelist definitions, not observed availability.

## Usage

``` r
tuik_decode_data(
  data,
  profile,
  dimensions = NULL,
  language = c("tr", "en", "both"),
  api_key = NULL
)
```

## Arguments

- data:

  A data frame returned by
  [`tuik_get_data()`](https://MFatihTuzen.github.io/tuiksdmx/reference/tuik_get_data.md).

- profile:

  A `tuik_flow_profile` object.

- dimensions:

  NULL (all coded dimensions present in `data`) or a character vector of
  dimension IDs to decode.

- language:

  One of `"tr"`, `"en"`, or `"both"`.

- api_key:

  Optional API key. If NULL, `TUIK_SDMX_API_KEY` is used.

## Value

A tibble containing the input columns plus `DIMENSION_label_tr` and/or
`DIMENSION_label_en` columns. Unknown code labels remain `NA`.

## Examples

``` r
if (FALSE) { # \dontrun{
decoded <- tuik_decode_data(data, profile, dimensions = c("DEGISIM", "FREQ"))
} # }
```

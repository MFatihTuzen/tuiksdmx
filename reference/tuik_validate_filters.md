# Validate SDMX filter values against referenced Codelists

Checks whether supplied filter values exist in the Codelists referenced
by the Dataflow's DSD.

## Usage

``` r
tuik_validate_filters(profile, filters, stop_on_error = TRUE, api_key = NULL)
```

## Arguments

- profile:

  A `tuik_flow_profile` object.

- filters:

  Named list of dimension selections.

- stop_on_error:

  Logical. If TRUE, stop when invalid codes are found.

- api_key:

  Optional API key. If NULL, the TUIK_SDMX_API_KEY environment variable
  is used.

## Value

A tibble with one row per supplied filter value.

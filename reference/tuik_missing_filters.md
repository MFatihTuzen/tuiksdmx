# Identify missing SDMX key filters

Compares a filter list with the dimensions required by the DSD.

## Usage

``` r
tuik_missing_filters(profile, filters)
```

## Arguments

- profile:

  A `tuik_flow_profile` object.

- filters:

  Named list of dimension selections.

## Value

Character vector containing missing dimension IDs.

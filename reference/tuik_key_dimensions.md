# Return the dimensions used in an SDMX series key

Extracts ordinary series dimensions from a TÜİK flow profile.

## Usage

``` r
tuik_key_dimensions(profile)
```

## Arguments

- profile:

  A `tuik_flow_profile` object.

## Value

A tibble containing the key dimensions in DSD order.

## Details

TimeDimension components such as TIME_PERIOD are excluded because they
are supplied separately through time query parameters.

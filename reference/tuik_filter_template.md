# Create a filter template for a TÜİK SDMX Dataflow

Creates a named list containing all dimensions required for an SDMX
series key. Each value is initially NULL and can be filled by the user.

## Usage

``` r
tuik_filter_template(profile)
```

## Arguments

- profile:

  A `tuik_flow_profile` object.

## Value

A named list with one element per series-key dimension.

## Details

TIME_PERIOD is excluded because time filtering is handled separately
through startPeriod and endPeriod.

## Examples

``` r
if (FALSE) { # \dontrun{
filters <- tuik_filter_template(profile)

filters$REF_AREA <- "TR"
filters$FREQ <- "M"
} # }
```

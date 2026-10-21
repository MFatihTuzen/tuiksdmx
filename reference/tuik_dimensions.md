# Extract dimensions from a TÜİK SDMX DSD

Parses Dimension and TimeDimension components from a DSD.

## Usage

``` r
tuik_dimensions(dsd)
```

## Arguments

- dsd:

  XML document returned by
  [`tuik_get_dsd()`](https://MFatihTuzen.github.io/tuiksdmx/reference/tuik_get_dsd.md).

## Value

A tibble describing DSD dimensions.

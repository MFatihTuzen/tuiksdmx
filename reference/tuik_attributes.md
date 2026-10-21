# Extract attributes from a TÜİK SDMX DSD

Parses attribute components and their coded representations from a DSD.

## Usage

``` r
tuik_attributes(dsd)
```

## Arguments

- dsd:

  XML document returned by
  [`tuik_get_dsd()`](https://MFatihTuzen.github.io/tuiksdmx/reference/tuik_get_dsd.md).

## Value

A tibble describing DSD attributes.

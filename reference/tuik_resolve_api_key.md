# Resolve a TUIK SDMX API key

Uses an explicitly supplied API key or, when NULL, the TUIK_SDMX_API_KEY
environment variable.

## Usage

``` r
tuik_resolve_api_key(api_key = NULL)
```

## Arguments

- api_key:

  An optional API key.

## Value

A non-empty character string.

# Refresh a cached TUIK SDMX access token

Requests a new access token and updates only the cache entry
corresponding to the selected API key.

## Usage

``` r
tuik_refresh_token(api_key = NULL)
```

## Arguments

- api_key:

  An optional API key.

## Value

The new access token invisibly.

## Details

If authentication fails, the existing cache entry remains unchanged.

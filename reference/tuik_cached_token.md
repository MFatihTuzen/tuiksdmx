# Get a cached TUIK SDMX access token

Returns the access token associated with the selected API key. If no
token is cached, requests a new one.

## Usage

``` r
tuik_cached_token(api_key = NULL)
```

## Arguments

- api_key:

  An optional API key.

## Value

A character string containing the access token.

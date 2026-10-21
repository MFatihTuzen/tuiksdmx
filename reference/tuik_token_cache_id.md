# Create a token cache identifier

Creates a SHA-256 identifier from an API key. The raw API key is not
used as a cache name.

## Usage

``` r
tuik_token_cache_id(api_key)
```

## Arguments

- api_key:

  A TUIK SDMX API key.

## Value

A SHA-256 hexadecimal character string.

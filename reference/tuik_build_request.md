# Build a TUIK SDMX request

Creates an authenticated HTTP request for a TUIK SDMX REST endpoint.

## Usage

``` r
tuik_build_request(path, query = list(), accept = NULL, api_key = NULL)
```

## Arguments

- path:

  Character string containing the REST path relative to the TUIK SDMX
  base URL.

- query:

  Named list of query parameters.

- accept:

  Optional HTTP Accept header.

- api_key:

  Optional API key. If NULL, the TUIK_SDMX_API_KEY environment variable
  is used.

## Value

An httr2 request object.

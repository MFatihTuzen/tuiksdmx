# Perform a TUIK SDMX request without stopping on HTTP errors

Intended mainly for diagnostics, tests and endpoint exploration.

## Usage

``` r
tuik_probe(path, query = list(), accept = NULL, timeout = 60, api_key = NULL)
```

## Arguments

- path:

  Character string containing the REST path.

- query:

  Named list of query parameters.

- accept:

  Optional HTTP Accept header.

- timeout:

  Request timeout in seconds.

- api_key:

  Optional API key. If NULL, the TUIK_SDMX_API_KEY environment variable
  is used.

## Value

An httr2 response object.

## Details

Unlike tuik_perform(), HTTP 4xx and 5xx responses are returned instead
of converted into R errors.

A single token refresh is attempted after HTTP 401.

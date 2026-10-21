# Perform a TUIK SDMX request

Performs an authenticated request against the TUIK SDMX REST service.

## Usage

``` r
tuik_perform(
  path,
  query = list(),
  accept = NULL,
  timeout = 60,
  max_tries = 3,
  api_key = NULL
)
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

- max_tries:

  Maximum number of attempts for temporary network failures.

- api_key:

  Optional API key. If NULL, the TUIK_SDMX_API_KEY environment variable
  is used.

## Value

An httr2 response object.

## Details

Behaviour:

- Uses an explicitly supplied or environment-based API key

- Retrieves the token associated with that API key

- Retries temporary connection failures

- Refreshes only the selected credential's token on HTTP 401

- Retries the request once after token refresh

- Converts remaining HTTP errors into readable R errors

# Get a new TUIK SDMX access token

Requests an access token from the TUIK authentication service.

## Usage

``` r
tuik_get_access_token(api_key = NULL)
```

## Arguments

- api_key:

  An optional API key. If NULL, the TUIK_SDMX_API_KEY environment
  variable is used.

## Value

An access-token character string. When the server supplies a positive
numeric `expires_in`, it is attached as an internal attribute.

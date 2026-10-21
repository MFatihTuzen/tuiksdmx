# Retrieve a TÜİK SDMX Data Structure Definition

Retrieves a DSD from the TÜİK SDMX Web Service.

## Usage

``` r
tuik_get_dsd(dsd_id, agency = "TR", version = "latest", api_key = NULL)
```

## Arguments

- dsd_id:

  Data Structure Definition identifier.

- agency:

  DSD agency. Defaults to "TR".

- version:

  DSD version. Defaults to "latest".

- api_key:

  Optional API key. If NULL, the TUIK_SDMX_API_KEY environment variable
  is used.

## Value

An xml2 XML document.

## Examples

``` r
if (FALSE) { # \dontrun{
dsd <- tuik_get_dsd(
  "DSD_TUFE",
  version = "1.12"
)
} # }
```

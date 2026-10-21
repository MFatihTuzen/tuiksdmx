# Retrieve a single TÜİK SDMX Dataflow

Retrieves metadata for one Dataflow and its referenced DSD.

## Usage

``` r
tuik_get_dataflow(
  dataflow_id,
  agency = "TR",
  version = "latest",
  api_key = NULL
)
```

## Arguments

- dataflow_id:

  Dataflow identifier.

- agency:

  Dataflow agency. Defaults to "TR".

- version:

  Dataflow version. Defaults to "latest".

- api_key:

  Optional API key. If NULL, the TUIK_SDMX_API_KEY environment variable
  is used.

## Value

A one-row tibble.

## Examples

``` r
if (FALSE) { # \dontrun{
tuik_get_dataflow(
  "DF_TUFE_SDMX_TT01",
  version = "1.0"
)
} # }
```

# Build a profile for a TÜİK SDMX Dataflow

Retrieves a Dataflow, follows its referenced DSD and parses its
structural components.

## Usage

``` r
tuik_flow_profile(
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

A list with the following components:

- dataflow

- dsd

- dimensions

- measures

- attributes

## Examples

``` r
if (FALSE) { # \dontrun{
profile <- tuik_flow_profile(
  "DF_TUFE_SDMX_TT01",
  version = "1.0"
)

profile$dimensions
} # }
```

# List TÜİK SDMX Dataflows

Retrieves the current Dataflow catalogue published by TÜİK.

## Usage

``` r
tuik_dataflows(
  agency = "TR",
  version = "latest",
  refresh = FALSE,
  api_key = NULL
)
```

## Arguments

- agency:

  Dataflow agency. Defaults to "TR".

- version:

  Version selector. Defaults to "latest".

- refresh:

  Logical. If TRUE, ignore the cached catalogue and retrieve a fresh
  copy from the service.

- api_key:

  Optional API key. If NULL, the TUIK_SDMX_API_KEY environment variable
  is used.

## Value

A tibble with one row per Dataflow.

## Details

The returned table contains:

- Dataflow agency, ID and version

- Final/non-final status

- Turkish and English names

- Referenced DSD agency, ID and version

Results are cached in memory for the current R session. Use
`refresh = TRUE` to force a new request to the TÜİK SDMX service.

## Examples

``` r
if (FALSE) { # \dontrun{
flows <- tuik_dataflows()

# Force a fresh download
flows <- tuik_dataflows(refresh = TRUE)
} # }
```

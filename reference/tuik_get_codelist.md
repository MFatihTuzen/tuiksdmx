# Retrieve a TÜİK SDMX Codelist

Downloads and parses an SDMX Codelist.

## Usage

``` r
tuik_get_codelist(
  codelist_id,
  agency,
  version = "latest",
  refresh = FALSE,
  api_key = NULL
)
```

## Arguments

- codelist_id:

  Codelist identifier.

- agency:

  Codelist agency.

- version:

  Codelist version.

- refresh:

  Logical. If TRUE, ignore the cache and download the Codelist again.

- api_key:

  Optional API key. If NULL, the TUIK_SDMX_API_KEY environment variable
  is used.

## Value

A tibble with code, Turkish label and English label.

## Details

Results are cached in memory for the current R session.

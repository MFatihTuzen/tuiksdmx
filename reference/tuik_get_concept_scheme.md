# Retrieve a TÜİK SDMX ConceptScheme

Downloads and parses a ConceptScheme. Results are cached in memory for
the current R session.

## Usage

``` r
tuik_get_concept_scheme(
  concept_scheme_id,
  agency,
  version = "latest",
  refresh = FALSE,
  api_key = NULL
)
```

## Arguments

- concept_scheme_id:

  ConceptScheme identifier.

- agency:

  ConceptScheme agency.

- version:

  ConceptScheme version. Defaults to "latest".

- refresh:

  Logical. If TRUE, ignore the cache and download again.

- api_key:

  Optional API key. If NULL, the TUIK_SDMX_API_KEY environment variable
  is used.

## Value

A tibble with Concept IDs and multilingual labels/descriptions.

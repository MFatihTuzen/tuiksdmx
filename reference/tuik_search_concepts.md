# Search concepts used by a TÜİK Dataflow

Searches component IDs, concept IDs, labels and descriptions in the
concepts referenced by a flow profile.

## Usage

``` r
tuik_search_concepts(
  profile,
  query,
  language = c("both", "tr", "en"),
  limit = 50,
  api_key = NULL
)
```

## Arguments

- profile:

  A `tuik_flow_profile` object.

- query:

  Search text.

- language:

  One of "both", "tr", or "en".

- limit:

  Maximum number of results. Use NULL for all matches.

- api_key:

  Optional API key. If NULL, the TUIK_SDMX_API_KEY environment variable
  is used.

## Value

A filtered concept tibble.

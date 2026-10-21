# Preload ConceptSchemes used by a Dataflow

Retrieves descendant structures for a Dataflow and stores all returned
ConceptSchemes in the in-memory ConceptScheme cache.

## Usage

``` r
tuik_preload_concepts(profile, refresh = FALSE, api_key = NULL)
```

## Arguments

- profile:

  A `tuik_flow_profile` object.

- refresh:

  Logical. If TRUE, overwrite already cached ConceptSchemes.

- api_key:

  Optional API key. If NULL, the TUIK_SDMX_API_KEY environment variable
  is used.

## Value

The number of ConceptSchemes stored in the cache, invisibly.

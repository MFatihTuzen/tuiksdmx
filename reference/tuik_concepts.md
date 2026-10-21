# Retrieve concepts used by a TÜİK Dataflow

Resolves the concepts referenced by dimensions, measures and attributes
in a flow profile and adds their Turkish/English labels and
descriptions.

## Usage

``` r
tuik_concepts(profile, refresh = FALSE, api_key = NULL)
```

## Arguments

- profile:

  A `tuik_flow_profile` object.

- refresh:

  Logical. If TRUE, refresh ConceptScheme metadata.

- api_key:

  Optional API key. If NULL, the TUIK_SDMX_API_KEY environment variable
  is used.

## Value

A tibble with component-to-concept mappings and semantic metadata.

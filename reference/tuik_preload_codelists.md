# Preload Codelists used by a Dataflow

Retrieves descendant structures for a Dataflow and stores all returned
Codelists in the in-memory Codelist cache.

## Usage

``` r
tuik_preload_codelists(profile, refresh = FALSE, api_key = NULL)
```

## Arguments

- profile:

  A `tuik_flow_profile` object.

- refresh:

  Logical. If TRUE, overwrite already cached Codelists.

- api_key:

  Optional API key. If NULL, the TUIK_SDMX_API_KEY environment variable
  is used.

## Value

The number of Codelists stored in the cache, invisibly.

## Details

This is more efficient than downloading each Codelist separately when
several dimensions need to be validated.

# Retrieve codes observed in a Dataflow for a time window

Returns values of a dimension that are actually present in data returned
by the selected Dataflow over a bounded period. This is intentionally
distinct from
[`tuik_dimension_codes()`](https://MFatihTuzen.github.io/tuiksdmx/reference/tuik_dimension_codes.md),
which returns the theoretical Codelist domain.

## Usage

``` r
tuik_available_codes(
  profile,
  dimension,
  start_period,
  end_period,
  filters = NULL,
  labels = TRUE,
  api_key = NULL,
  query_mode = c("auto", "all"),
  allow_all = FALSE
)
```

## Arguments

- profile:

  A `tuik_flow_profile` object.

- dimension:

  Dimension ID whose observed values should be returned.

- start_period:

  Required start period.

- end_period:

  Required end period.

- filters:

  Optional named list of dimension filters. Filter values are validated
  against Codelists, and reapplied locally to the returned data.

- labels:

  Logical. If TRUE, attach Turkish and English Codelist labels when the
  dimension references a Codelist.

- api_key:

  Optional API key. If NULL, the TUIK_SDMX_API_KEY environment variable
  is used.

- query_mode:

  `"auto"` (default) uses a complete server-side SDMX key when possible.
  `"all"` always requests all series for the period. The `all` path
  requires `allow_all = TRUE` in either mode.

- allow_all:

  Logical. Explicitly authorizes a potentially large `all` download if
  filters are partial/absent or `query_mode = "all"`. This does NOT
  impose a byte, row, memory, or timeout limit.

## Value

A tibble with observed code values, row counts and optional labels.

## Details

By default `query_mode = "auto"` uses a server-side key for complete
filters. Partial or absent filters require the explicit
`allow_all = TRUE` opt-in because the service may return a large `all`
response. `query_mode = "all"` also requires `allow_all = TRUE`. Both
period bounds are required. Period bounds do not impose a limit on
response size or memory usage. This is observed data availability, not
an SDMX ContentConstraint.

## Examples

``` r
if (FALSE) { # \dontrun{
tuik_available_codes(
  profile,
  dimension = "COICOP_2018",
  start_period = "2026-01",
  end_period = "2026-03",
  allow_all = TRUE  # No filters: explicit broad-download opt-in
)

tuik_available_codes(
  profile,
  dimension = "COICOP_2018",
  start_period = "2026-01",
  end_period = "2026-03",
  filters = list(FREQ = "M", REF_AREA = "TR"),
  allow_all = TRUE  # Partial filters: downloaded data are filtered locally
)
} # }
```

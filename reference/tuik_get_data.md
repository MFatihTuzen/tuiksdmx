# Retrieve data from a TÜİK SDMX Dataflow

Retrieves all series with `filters = NULL`, or builds a complete SDMX
key from supplied filters. Partial filters require explicit permission
via `allow_all = TRUE` or `filter_mode = "local"`: the service's `all`
data are retrieved first and filters are applied in R. At least one
period bound is required unless `allow_unbounded = TRUE`. This is NOT
server-side filtering and can download large responses even for a short
period. Complete filters are always queried by SDMX key on the server.

## Usage

``` r
tuik_get_data(
  profile,
  filters = NULL,
  start_period = NULL,
  end_period = NULL,
  parse_observation = TRUE,
  validate = TRUE,
  api_key = NULL,
  allow_unbounded = FALSE,
  labels = c("none", "tr", "en", "both"),
  label_dimensions = NULL,
  filter_mode = c("reject", "local"),
  allow_all = FALSE
)
```

## Arguments

- profile:

  A `tuik_flow_profile` object or a single Dataflow ID. When an ID is
  supplied, a profile is resolved automatically with
  [`tuik_flow_profile()`](https://MFatihTuzen.github.io/tuiksdmx/reference/tuik_flow_profile.md)
  using the same `api_key`. For explicit Dataflow agency/version
  selection, construct a profile first.

- filters:

  NULL to request all series, or a named list of key-dimension
  selections. Partial selections require `allow_all = TRUE` (or legacy
  `filter_mode = "local"`) before the service's `all` data are
  downloaded.

- start_period:

  Optional start period, for example "2026-01".

- end_period:

  Optional end period, for example "2026-03".

- parse_observation:

  Logical. If TRUE, attempts to convert OBS_VALUE to numeric while
  preserving all SDMX dimensions and attributes as character columns.

- validate:

  Logical. If TRUE, validate supplied filter values against the
  Codelists referenced by the Dataflow DSD before requesting data.
  Defaults to TRUE.

- api_key:

  Optional API key. If NULL, the TUIK_SDMX_API_KEY environment variable
  is used.

- allow_unbounded:

  Logical. Must be TRUE to permit unfiltered or locally filtered
  requests with neither period bound supplied. This can download a very
  large dataset. Does not limit the server response size.

- labels:

  Language of optional Codelist labels: `"none"` (the default), `"tr"`,
  `"en"`, or `"both"`. Raw SDMX codes remain unchanged.

- label_dimensions:

  Optional character vector of dimension IDs to decode when `labels` is
  not `"none"`. If NULL, decode all coded dimensions present in the
  downloaded data.

- filter_mode:

  Legacy explicit mode: `"reject"` (default) or `"local"`. Existing
  calls remain supported. For new code, prefer `allow_all = TRUE` to
  authorize a locally filtered `all` request.

- allow_all:

  Logical, default FALSE. With incomplete `filters`, TRUE authorizes an
  `all` request and applies the specified filters locally. This is the
  same opt-in used by
  [`tuik_available_codes()`](https://MFatihTuzen.github.io/tuiksdmx/reference/tuik_available_codes.md).
  It DOES NOT limit transferred bytes, memory, or response time. It does
  not bypass `allow_unbounded` when neither period bound is provided.

## Value

A tibble containing SDMX observations. If labels are requested,
additional `<DIMENSION>_label_tr` and/or `<DIMENSION>_label_en` columns
are appended. Labels identify Codelist-defined values, not observed
availability.

## Examples

``` r
if (FALSE) { # \dontrun{
data <- tuik_get_data(
  profile,
  filters = list(
    REF_AREA = "TR",
    FREQ = "M",
    SINIFLAMA_DUZEYI = "2",
    DEGISIM = "1",
    OZEL_KAPSAM_TUFE = "_Z",
    BASE_PER = "2025",
    YAYIM_DONEMI = "2026_01",
    COICOP_1999 = "_Z",
    COICOP_2018 = "01",
    INDICATOR = "F_TFE"
  ),
  start_period = "2026-01",
  end_period = "2026-03"
)
} # }
```

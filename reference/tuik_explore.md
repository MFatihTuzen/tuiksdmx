# Explore a TUIK SDMX Dataflow before downloading observations

Displays structural components from a Dataflow's DSD without downloading
observations. Series dimensions are selected using `filters`;
TIME_PERIOD uses `start_period` and `end_period` in
[`tuik_get_data()`](https://MFatihTuzen.github.io/tuiksdmx/reference/tuik_get_data.md).
A specific coded dimension can be inspected on demand. Codelist entries
are theoretical, not necessarily observed in a selected time window.

## Usage

``` r
tuik_explore(
  dataflow,
  dimension = NULL,
  search = NULL,
  language = c("both", "tr", "en"),
  limit = 50,
  api_key = NULL,
  filters = NULL
)
```

## Arguments

- dataflow:

  A Dataflow ID or a `tuik_flow_profile` object.

- dimension:

  Optional series dimension ID whose codes to display.

- search:

  Optional search text for `dimension` codes.

- language:

  One of `"both"`, `"tr"`, or `"en"`.

- limit:

  Maximum codes to display for a selected dimension; default 50.

- api_key:

  Optional API key; defaults to `TUIK_SDMX_API_KEY`.

- filters:

  Optional named list of selected series-dimension codes. When supplied
  without `dimension`, adds `selected_codes` and `filter_status` columns
  to the DSD overview. This reports completeness only; it does not
  validate Codelist membership or download observations. Pass the same
  filters to
  [`tuik_get_data()`](https://MFatihTuzen.github.io/tuiksdmx/reference/tuik_get_data.md),
  which validates them before retrieval.

## Value

Without `dimension`, a tibble describing DSD components and how to
select them. If `filters` is supplied, also shows selections and missing
series dimensions. With `dimension`, a tibble of Codelist codes and
labels.

## Examples

``` r
if (FALSE) { # \dontrun{
tuik_explore("DF_TUFE_SDMX_TT01")
tuik_explore("DF_TUFE_SDMX_TT01", dimension = "DEGISIM")
selections <- list(REF_AREA = "TR", FREQ = "M", DEGISIM = "1")
tuik_explore("DF_TUFE_SDMX_TT01", filters = selections)
data <- tuik_get_data("DF_TUFE_SDMX_TT01", filters = selections,
                      filter_mode = "local", start_period = "2026-01",
                      end_period = "2026-01")
} # }
```

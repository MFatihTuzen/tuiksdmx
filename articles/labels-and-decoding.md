# Labels and decoding SDMX data

## Why keep codes and add labels?

SDMX uses stable **dimension codes** in series keys. People analysing
the results often prefer readable descriptions. `tuiksdmx` can add
Turkish and/or English labels from the DSD-referenced Codelists
**without overwriting the original code columns**. That matters for
reproducibility, joins and future queries. Labels describe *defined*
codes; they do not establish that a code has observations for every
period.

This article follows the CPI Dataflow `DF_TUFE_SDMX_TT01`, as tested
during development. Live structures, translations and returned
observations may change. All examples are intentionally **not executed**
when this vignette is built. Set `TUIK_SDMX_API_KEY` securely (for
example, in your personal `.Renviron`) or supply `api_key = api` to
authenticated calls.

``` r

library(tuiksdmx)
id <- "DF_TUFE_SDMX_TT01"
profile <- tuik_flow_profile(id)
```

## 1. Start with raw codes

Use a short observation window for a first example, but remember that a
bounded request with no `filters` still retrieves **all series** in that
window. A period bound is not a maximum row count.

``` r

raw <- tuik_get_data(
  profile,
  start_period = "2026-01",
  end_period = "2026-01"
)

# The original coded dimension values are preserved
raw[, intersect(c("FREQ", "DEGISIM", "COICOP_2018", "TIME_PERIOD", "OBS_VALUE"),
                names(raw)), drop = FALSE]
```

## 2. Add Turkish, English or both labels at retrieval time

`labels` has four permitted values: `"none"` (default), `"tr"`, `"en"`,
`"both"`. For a focused example, use `label_dimensions` to decode only
the needed dimensions; leaving it `NULL` requests labels for all coded
dimensions present in the returned data.

``` r

cpi_tr <- tuik_get_data(
  profile,
  start_period = "2026-01",
  end_period = "2026-01",
  labels = "tr",
  label_dimensions = c("FREQ", "DEGISIM")
)

# The code column and its Turkish label appear side by side
head(cpi_tr[, c("DEGISIM", "DEGISIM_label_tr"), drop = FALSE])

cpi_both <- tuik_get_data(
  profile,
  start_period = "2026-01",
  end_period = "2026-01",
  labels = "both",
  label_dimensions = c("DEGISIM", "COICOP_2018")
)

head(cpi_both[, c("DEGISIM", "DEGISIM_label_tr", "DEGISIM_label_en"),
              drop = FALSE])
```

Each new name takes the form `DIMENSION_label_tr` or
`DIMENSION_label_en`. For example, `COICOP_2018_label_tr` accompanies
`COICOP_2018`; `TIME_PERIOD` and `OBS_VALUE` are not Codelist label
columns. Labels are **additional columns**, not replacements for the
SDMX keys.

## 3. Decode data you have already downloaded

You do not have to repeat observation retrieval merely to change the
label language.
[`tuik_decode_data()`](https://MFatihTuzen.github.io/tuiksdmx/reference/tuik_decode_data.md)
accepts an existing data frame and the corresponding profile:

``` r

decoded <- tuik_decode_data(
  raw,
  profile,
  dimensions = c("FREQ", "DEGISIM"),
  language = "both"
)

head(decoded[, c("FREQ", "FREQ_label_tr", "FREQ_label_en"),
             drop = FALSE])
```

The function matches observation codes against Codelist definitions and
preserves the existing observations and their row count. It may still
need metadata requests to fetch Codelists; *decoding an existing table*
avoids re-downloading observations, not necessarily every network call.

## 4. Choose dimensions from metadata, not guesswork

Inspect the Dataflow’s key dimensions and code domain before requesting
labels:

``` r

tuik_filter_dimensions(profile)
tuik_dimension_codes(profile, dimension = "DEGISIM")

# Inspect codes/descriptions in the interactive-style helper
tuik_explore(profile, dimension = "COICOP_2018", search = "gıda")
```

Not every structural component refers to a Codelist. `dimensions` in
[`tuik_decode_data()`](https://MFatihTuzen.github.io/tuiksdmx/reference/tuik_decode_data.md)
and `label_dimensions` in
[`tuik_get_data()`](https://MFatihTuzen.github.io/tuiksdmx/reference/tuik_get_data.md)
must name coded dimensions actually present in the data. Specifying an
unknown or uncoded dimension results in an error rather than a
fabricated description.

## 5. Labels do not prove availability

A label comes from a *theoretical Codelist domain*. It is not evidence
that a code has data for January 2026 or for a chosen filter
combination.

``` r

# Defined domain: Codelist codes and descriptions
possible <- tuik_dimension_codes(profile, dimension = "COICOP_2018")

# Observed domain: codes in returned observation rows for a bounded query
observed <- tuik_available_codes(
  profile,
  dimension = "COICOP_2018",
  start_period = "2026-01",
  end_period = "2026-01",
  allow_all = TRUE
)
```

The observed query above explicitly authorizes an `all` download. It may
be costly even with one month selected.
[`tuik_available_codes()`](https://MFatihTuzen.github.io/tuiksdmx/reference/tuik_available_codes.md)
reports codes observed in the retrieved rows; it does **not** fetch SDMX
ContentConstraint metadata.

## 6. Common pitfalls

- `labels = "none"` together with `label_dimensions = ...` is invalid:
  ask for `"tr"`, `"en"` or `"both"` first.
- Unknown translations can yield `NA` label values; they do not alter
  the original SDMX codes.
- Do not convert dimension code columns to numeric before decoding.
  Codes are identifiers and may contain leading zeros or nonnumeric
  symbols.
- Calling
  [`tuik_decode_data()`](https://MFatihTuzen.github.io/tuiksdmx/reference/tuik_decode_data.md)
  again for the same dimensions/language on an already decoded table can
  raise a name-collision error. Decode from `raw` or select new,
  nonconflicting label columns.
- Labels do not reduce response size. Use a **complete DSD key** when
  you need more selective server-side retrieval, as described in
  *Filtering, SDMX keys and performance*.

## Next steps

Start with *Getting started* for discovery and retrieval, *Metadata and
DSD* for the meaning of dimensions and Codelists, and *Observed
availability* for the distinction between valid and actually observed
codes.

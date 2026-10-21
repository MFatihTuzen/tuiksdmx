# tuiksdmx

<!-- README: first public-facing draft, 20 October 2026. -->

**An authenticated, metadata-driven R client for the Turkish Statistical Institute (TÜİK / TURKSTAT) SDMX REST service.**

`tuiksdmx` helps you discover statistical datasets, inspect their structure and codes, and retrieve observations as R tables. It supports both a short route for everyday use and DSD-aware workflows for reproducible, precisely filtered requests.

> **Development status:** `0.0.0.9000` is a development version. The package is not presented here as available from CRAN. TÜİK SDMX endpoints require authentication. Examples below use a real Dataflow ID verified during development, but service content, structures, and access conditions can change.

## What you can do

- Find Dataflows and inspect their dimensions **before downloading observations**.
- Examine dimension codes and their Turkish/English descriptions.
- Download data by supplying a Dataflow ID, or reuse a Dataflow profile for more detailed work.
- Select observation periods with `start_period` and `end_period`.
- Use complete SDMX keys for targeted retrieval, or explicitly allow broad downloads followed by local filtering.
- Preserve original SDMX codes and optionally add Codelist labels.
- Distinguish codes defined by a Codelist from codes **observed** in data for a specified period.

## Installation

For now, install from a local checkout of the package project:

```r
# Run in the root directory of the tuiksdmx package
install.packages("pak")                 # only if needed
pak::local_install(".")
```

For package development, you can alternatively use `devtools::load_all()` from the project directory. Public repository installation instructions will be added when a release location is confirmed.

## Authentication

The TÜİK SDMX service requires an API key. **You can provide it in three ways.**
The recommended option for regular use is an environment variable, but
neither `.Renviron` nor the variable name is mandatory when you pass the
key explicitly with `api_key`.

**Option A — Persistent setting (recommended).** Add the following to your
personal `.Renviron` file (for example, open it with `usethis::edit_r_environ()`):

```text
TUIK_SDMX_API_KEY=YOUR_OWN_KEY
```

Restart R after saving the file. The package reads this variable when
`api_key` is omitted, so normal calls remain short:

```r
cpi <- tuik_get_data("DF_TUFE_SDMX_TT01",
                     start_period = "2026-01", end_period = "2026-01")
```

**Option B — Pass a key explicitly.** An R object can have *any name*, such
as `api`. The code below assumes the object has already been created using a
secure, untracked method; do not save real keys in scripts or notebooks.

```r
# api <- <your securely obtained character-string API key>
cpi <- tuik_get_data("DF_TUFE_SDMX_TT01",
                     start_period = "2026-01", end_period = "2026-01",
                     api_key = api)
```

The `api_key` argument is accepted by authenticated discovery, exploration,
profile, metadata and retrieval functions as well. An explicitly supplied
`api_key` takes precedence over `TUIK_SDMX_API_KEY`.

**Option C — Set the variable for the current R session.** When `api` already
contains a securely supplied key, you can set:

```r
Sys.setenv(TUIK_SDMX_API_KEY = api)
# Subsequent calls can omit api_key.
```

`Sys.setenv()` is session-scoped; it does not write to `.Renviron`.
Regardless of the method, **never commit credentials, print token values,
include them in bug reports, or share `.Renviron`**. Typing credentials
in an ordinary R console or `readline()` may expose them on screen or in
session history; prefer environment-based or secret-input workflows.

## Quick start: download a period

If you already know a Dataflow ID, you can retrieve observations without manually creating an SDMX key:

```r
library(tuiksdmx)

cpi <- tuik_get_data(
  "DF_TUFE_SDMX_TT01",
  start_period = "2026-01",
  end_period = "2026-01"
)

head(cpi)
```

This example requests **all series within the chosen period**, not just a single CPI indicator. During development this request returned 84 rows and 21 columns; that is an observation from a particular test, not a permanent guarantee.

### Explore first; download second

```r
# Inspect the structure without downloading observations
tuik_explore("DF_TUFE_SDMX_TT01")

# Inspect available codes and descriptions for a series dimension
tuik_explore("DF_TUFE_SDMX_TT01", dimension = "DEGISIM")

# Find a code by its description
tuik_explore(
  "DF_TUFE_SDMX_TT01",
  dimension = "COICOP_2018",
  search = "gıda"
)
```

`TIME_PERIOD` is the observation time dimension. It is selected with `start_period` / `end_period`; it is **not** included in the SDMX series key or the `filters` list. `YAYIM_DONEMI` is a *different* DSD series dimension and should not be confused with `TIME_PERIOD`.

### Download with a few filters

You may specify only the dimensions you care about:

```r
chosen <- list(
  REF_AREA = "TR",
  FREQ = "M",
  DEGISIM = "1",
  SINIFLAMA_DUZEYI = "2"
)

# Review which key dimensions were selected or left unspecified
tuik_explore("DF_TUFE_SDMX_TT01", filters = chosen)

cpi_selected <- tuik_get_data(
  "DF_TUFE_SDMX_TT01",
  filters = chosen,
  start_period = "2026-01",
  end_period = "2026-01",
  allow_all = TRUE,
  labels = "tr"
)
```

**Important:** When `filters` does not specify **every** series key dimension, `allow_all = TRUE` permits downloading *all series for the requested time range*, then filtering in R. The number of returned rows may be small even though the download was large. This option is explicit because the TÜİK service did **not** accept the partial-key patterns tried during package development. `allow_all = TRUE` is **not a memory, response-size, or time limit**.

For efficient server-side selection, provide a complete set of DSD key dimensions. See the [Getting started vignette](vignettes/getting-started.Rmd) for the full-key route.

## Advanced use: profile, codes, and observed availability

```r
profile <- tuik_flow_profile("DF_TUFE_SDMX_TT01")

# The series key dimensions, their positions, and referenced Codelists
tuik_filter_dimensions(profile)

# Theoretical domain: codes defined in the referenced Codelist
tuik_dimension_codes(profile, dimension = "DEGISIM")

# Codes genuinely observed for this specific request window
observed <- tuik_available_codes(
  profile,
  dimension = "COICOP_2018",
  filters = list(
    REF_AREA = "TR",
    FREQ = "M",
    SINIFLAMA_DUZEYI = "2",
    DEGISIM = "1"
  ),
  start_period = "2026-01",
  end_period = "2026-01",
  allow_all = TRUE
)

observed
```

The Codelist domain is **not** the same as observed availability. `tuik_available_codes()` calculates observed codes from returned observations; it does **not** claim to implement SDMX ContentConstraint.

## Safe downloading and performance

| Situation | Recommended behavior |
|---|---|
| No filters, period specified | `tuik_get_data(id, start_period = ..., end_period = ...)` |
| Partial key dimensions | Supply `allow_all = TRUE` deliberately; local filtering follows a potentially broad download |
| Complete key dimensions | `tuik_get_data()` builds a targeted SDMX key; no `allow_all` needed |
| No period bounds, all/partial series | `allow_unbounded = TRUE` is additionally required |
| Same Dataflow used repeatedly | Reuse `profile <- tuik_flow_profile(id)` across calls |

Requests can take less than a second or over a minute depending on server, network, authentication and query conditions. During development some identical small requests took approximately 1 second and others about 62 seconds; the exact cause of those intermittent delays was **not** confirmed. Neither `allow_all` nor a short time window guarantees a small data response.

## Guides and vignettes

The following guides are provided under `vignettes/` and can be built with `devtools::build_vignettes()`:

- [Getting started](vignettes/getting-started.Rmd) — discovery, exploration, periods, filtering, retrieval and labels.
- [Metadata and DSD](vignettes/metadata-and-dsd.Rmd) — Dataflows, structure definitions, dimensions, concepts and Codelists.
- [Filtering and performance](vignettes/filtering-and-performance.Rmd) — full versus partial keys, explicit opt-ins, response size and request latency.
- [Observed availability](vignettes/observed-availability.Rmd) — theoretical Codelist domains versus codes actually observed in selected data.
- [Authentication and secure use](vignettes/authentication.Rmd) — `.Renviron`, `Sys.setenv()`, explicit `api_key`, token handling and safe reproducible examples.

## Scope and limitations

`tuiksdmx` focuses on the authenticated SDMX REST workflow. It is not intended to reproduce broader portal discovery, downloadable spreadsheet resources or geography facilities of `tuikr`.

Server-side wildcard/partial SDMX keys, SDMX ContentConstraint querying, streaming and guaranteed hard limits on download size are **not claimed** as supported features. Functionality has been empirically checked on selected CPI and birth Dataflows; that does not prove all TÜİK Dataflows behave identically.

## Development quality

As last reported on **20 October 2026**, the local test suite recorded **534 passing expectations**. `devtools::check()` reported **0 errors, 0 warnings and 1 note** (`unable to verify current time` during future-file-timestamp verification on one Windows environment). These describe a development snapshot, not a release certification.

## License

MIT (see `LICENSE`).

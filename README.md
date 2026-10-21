<div align="center">

# tuiksdmx

**An authenticated, metadata-driven R client for the TÜİK (Turkish Statistical Institute) SDMX REST API.**

Discover dataflows · Understand structures · Build validated queries · Retrieve statistical data

[![R-CMD-check](https://github.com/MFatihTuzen/tuiksdmx/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MFatihTuzen/tuiksdmx/actions/workflows/R-CMD-check.yaml)
[![Documentation](https://img.shields.io/badge/docs-pkgdown-AB2328.svg)](https://mfatihtuzen.github.io/tuiksdmx/)
[![License: MIT](https://img.shields.io/badge/license-MIT-AB2328.svg)](LICENSE.md)
[![R ≥ 4.1.0](https://img.shields.io/badge/R-%E2%89%A5%204.1.0-555555.svg)](DESCRIPTION)
[![Status: Development](https://img.shields.io/badge/status-development-AB2328.svg)](NEWS.md)

[Get started](#quick-start) · [Installation](#installation) · [Authentication](#authentication) · [Guides](#guides) · [Documentation](https://mfatihtuzen.github.io/tuiksdmx/) · [License](#license)

</div>

> **Development release (`0.0.0.9000`).** `tuiksdmx` is currently distributed through GitHub, not CRAN. TÜİK SDMX endpoints require authentication. Dataflow structures and service behaviour may change; examples below use a Dataflow verified during development.

## Why tuiksdmx?

SDMX datasets are multidimensional. To construct a meaningful query, you may need to know the Dataflow, its data structure definition (DSD), dimension order, codes and time range. **`tuiksdmx` uses this metadata to make the process easier and more reproducible**, without requiring you to manually assemble an SDMX key for ordinary workflows.

| Discover | Understand | Query | Retrieve |
|:--|:--|:--|:--|
| Search Dataflows and inspect their metadata | Explore DSD dimensions, concepts, codelists and labels | Validate filters and construct structure-aware keys | Download observations and optionally decode labels |

The package also offers **observed availability**: inspect which dimension codes occur in data for a specified request, rather than assuming every code in a codelist has observations.

## Installation

Install the development version from GitHub using **pak** (recommended):

```r
install.packages("pak")  # Run once, if not already installed
pak::pak("MFatihTuzen/tuiksdmx")
```

Alternatively, use **remotes**, a familiar option for installing R packages from GitHub:

```r
install.packages("remotes")  # Run once, if not already installed
remotes::install_github("MFatihTuzen/tuiksdmx")
```

Both methods install the GitHub development version; the package is not yet on CRAN. Then load it:

```r
library(tuiksdmx)
```

For contributors working from a local source checkout, `devtools::load_all()` is also available.

## Quick start

### 1. Configure authentication

TÜİK SDMX requests require an API key. For everyday use, store it in your personal `.Renviron` file (open it with `usethis::edit_r_environ()` if needed):

```text
TUIK_SDMX_API_KEY=YOUR_OWN_API_KEY
```

Restart R after saving. The package will read this environment variable automatically. **Never commit credentials** or paste access tokens into issues, scripts or notebooks.

You can also supply an API key explicitly through `api_key` or set `TUIK_SDMX_API_KEY` for the current session; see [Authentication options](#authentication).

### 2. Discover Dataflows

You do **not** need to know a Dataflow ID in advance. Browse the authenticated catalogue or search by subject:

```r
# Retrieve the catalogue of Dataflows
flows <- tuik_dataflows()

# View identifiers and their Turkish / English names
head(flows[c("dataflow_id", "name_tr", "name_en")], 10)

# Search the catalogue in Turkish (accent-insensitive matching)
tuik_search_dataflows("tüketici fiyat", limit = 10)

# Or search by English terminology
tuik_search_dataflows("consumer price", language = "en", limit = 10)
```

Choose a `dataflow_id` from the results. The remainder of this example uses the CPI Dataflow `DF_TUFE_SDMX_TT01`, which was inspected during package development; live catalogues can change.

### 3. Explore a Dataflow

```r
# Inspect dimensions and metadata without retrieving observations
tuik_explore("DF_TUFE_SDMX_TT01")

# List codes and their descriptions for a dimension
tuik_explore("DF_TUFE_SDMX_TT01", dimension = "DEGISIM")

# Search descriptions in another dimension
tuik_explore(
  "DF_TUFE_SDMX_TT01",
  dimension = "COICOP_2018",
  search = "gıda"
)
```

### 4. Retrieve observations

If you know the Dataflow ID and time window, you can make a request directly:

```r
cpi <- tuik_get_data(
  "DF_TUFE_SDMX_TT01",
  start_period = "2026-01",
  end_period   = "2026-01"
)

head(cpi)
```

This requests **all series for the specified observation period**. It is not limited to a single CPI indicator.

### 5. Narrow the result with structured filters

```r
chosen <- list(
  REF_AREA = "TR",
  FREQ = "M",
  DEGISIM = "1",
  SINIFLAMA_DUZEYI = "2"
)

# Check the filters against the structure
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

> **Caution — partial filters can trigger broad downloads.** With a partial series key, `allow_all = TRUE` explicitly permits retrieval of all series within the requested period, followed by **local filtering in R**. It is **not** a download-size, memory or time limit. The partial/wildcard key patterns tested during development were not accepted by the TÜİK service. For targeted server-side requests, specify a complete set of series key dimensions.

**Time dimension detail:** `TIME_PERIOD` refers to observation time and is controlled with `start_period` / `end_period`, not placed in the series-key `filters`. `YAYIM_DONEMI` is a separate DSD series dimension.

## Metadata, profiles and observed availability

Use a profile when inspecting or querying the same Dataflow repeatedly:

```r
profile <- tuik_flow_profile("DF_TUFE_SDMX_TT01")

# Series-key dimensions and their associated codelists
tuik_filter_dimensions(profile)

# Theoretical domain defined by a codelist
tuik_dimension_codes(profile, dimension = "DEGISIM")

# Codes actually present in the returned observations for this request
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

**Codelist domain ≠ observed availability.** A codelist describes permissible codes; `tuik_available_codes()` derives observed codes from retrieved observations for the specified request. It does **not** implement SDMX ContentConstraint queries.

## Authentication

The package supports three ways to provide an API key:

| Method | Example | When to use it |
|:--|:--|:--|
| **Environment variable (recommended)** | Set `TUIK_SDMX_API_KEY` in your personal `.Renviron` | Regular, reproducible use without keys in scripts |
| **Explicit argument** | `tuik_get_data(id, ..., api_key = api)` | When a key is supplied securely by your application |
| **Current-session environment** | `Sys.setenv(TUIK_SDMX_API_KEY = api)` | Temporary sessions; does not edit `.Renviron` |

An explicit `api_key` takes precedence over the environment variable. It is also supported by authenticated discovery, exploration, profiles and metadata operations. The object named `api` above is a placeholder for a key obtained securely; do not store it in tracked scripts or print credentials or tokens.

## Download safeguards and performance

| Request | Behaviour |
|:--|:--|
| A Dataflow ID with a bounded time range and no series filters | Downloads all series within that period |
| Complete DSD series-key filters | Builds a targeted server-side SDMX key |
| Partial series-key filters | Requires deliberate `allow_all = TRUE`, with potentially broad retrieval and subsequent local filtering |
| Unbounded request involving all/partial series | Additionally requires `allow_unbounded = TRUE` |
| Repeated requests against one Dataflow | Reuse `tuik_flow_profile()` to avoid repeated structure discovery where possible |

Response times can vary considerably. Selected development requests took approximately one second on some attempts and over a minute on others; the cause was not confirmed. A short period or `allow_all = TRUE` does not guarantee a small response.

## Guides

The README is a starting point, not a complete function reference. Six guides are included in the package and available on the [documentation website](https://mfatihtuzen.github.io/tuiksdmx/articles/):

| Guide | What it covers |
|:--|:--|
| [Getting started](https://mfatihtuzen.github.io/tuiksdmx/articles/getting-started.html) | Discovery, exploration, periods, filtering and retrieval |
| [Metadata and DSD](https://mfatihtuzen.github.io/tuiksdmx/articles/metadata-and-dsd.html) | Dataflows, dimensions, DSDs, concepts and codelists |
| [Filtering and performance](https://mfatihtuzen.github.io/tuiksdmx/articles/filtering-and-performance.html) | Full vs partial keys, safeguards and request performance |
| [Observed availability](https://mfatihtuzen.github.io/tuiksdmx/articles/observed-availability.html) | Codelist domains versus observed codes |
| [Authentication and secure use](https://mfatihtuzen.github.io/tuiksdmx/articles/authentication.html) | API key options, tokens and reproducible credential handling |
| [Labels and decoding](https://mfatihtuzen.github.io/tuiksdmx/articles/labels-and-decoding.html) | Turkish/English labels, dimension selection and decoding data already retrieved |

After installation, use `browseVignettes("tuiksdmx")` to view built guides in R. For individual function arguments and examples, open R help pages such as `?tuik_get_data`, `?tuik_search_dataflows`, and `?tuik_decode_data`.

**Labels and readable outputs:** `tuik_get_data(..., labels = "tr")` can add Turkish code descriptions; `labels = "en"` or `"both"` are also supported. The `label_dimensions` argument controls which dimensions are decoded. See the [Labels and decoding guide](https://mfatihtuzen.github.io/tuiksdmx/articles/labels-and-decoding.html) for a worked example.

The [pkgdown documentation website](https://mfatihtuzen.github.io/tuiksdmx/) brings together the [function reference](https://mfatihtuzen.github.io/tuiksdmx/reference/), all six guides and the changelog.

## Scope and limitations

`tuiksdmx` is a **focused SDMX client**. It is not intended to duplicate broader TÜİK portal resource discovery, Excel downloads or geographic functionality provided by other tooling such as `tuikr`.

The package does **not** claim support for server-side partial/wildcard SDMX keys, ContentConstraint querying, streaming, or guaranteed hard limits on downloads. Selected CPI and birth Dataflows were used during development; success with these does not establish compatibility with every Dataflow or future service configuration.

## Development and quality

The project has a `testthat` suite and an automated [R CMD check workflow](https://github.com/MFatihTuzen/tuiksdmx/actions/workflows/R-CMD-check.yaml) for Ubuntu, Windows and macOS. The latest local development validation reported **534 passing expectations**, **0 errors**, **0 warnings** and **1 note** (the local environment could not verify its current time). The subsequent three-platform CI run completed successfully; results are checks of this development snapshot, not a guarantee of live API availability.

Contributions and reproducible bug reports can be shared through [GitHub Issues](https://github.com/MFatihTuzen/tuiksdmx/issues). **Do not attach credentials, tokens or private session files.**

## License

`tuiksdmx` is distributed under the **[MIT License](LICENSE.md)**.

# Changelog

## tuiksdmx 0.1.1

- Sanitize authentication HTTP and JSON parsing errors so credentials
  and sensitive response contents are not exposed in error conditions.
- Require testthat \>= 3.1.7 for the mocked test suite.
- Make SDMX help examples self-contained while retaining
  authenticated-example safeguards.
- Exclude local credentials, private keys, logs, caches and development
  archives from source builds.
- Add Linux R-devel CRAN validation, including indexed PDF manual
  checks.

## tuiksdmx 0.1.0

### Initial release

- Add authenticated access to the TÜİK SDMX REST service, including
  token management.
- Support Dataflow discovery and DSD, concept, and codelist exploration.
- Build and validate metadata-driven SDMX queries, and retrieve
  observations.
- Support observation-period selection and optional label decoding.
- Explore observed dimension values separately from theoretical codelist
  domains.
- Document safeguards for broad downloads and performance
  considerations.
- Add six introductory and advanced workflow vignettes and offline
  tests.
- Verify small authenticated live API requests for labour force and CPI
  Dataflows.
- Add the package hex sticker logo and publication-ready documentation.

The 0.1.0 series is distributed from GitHub. See GitHub Releases for
published tags; no CRAN publication is claimed.

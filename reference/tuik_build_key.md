# Build an SDMX data key

Builds a series key using the exact dimension order defined by the DSD.

## Usage

``` r
tuik_build_key(profile, filters)
```

## Arguments

- profile:

  A `tuik_flow_profile` object.

- filters:

  Named list of dimension selections.

## Value

Character string containing the SDMX key.

## Details

Each dimension may contain:

- one value, e.g. "M"

- multiple values, e.g. c("01", "02")

Multiple values are joined using the SDMX "+" separator.

At the current stage, all series dimensions must be supplied explicitly.
This is intentional because blank-segment wildcard behaviour has not
been reliably confirmed for the TÜİK service.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires TUIK_SDMX_API_KEY in the environment.
library(tuiksdmx)
profile <- tuik_flow_profile("DF_TUFE_SDMX_TT01")

key <- tuik_build_key(
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
    COICOP_2018 = c("01", "02"),
    INDICATOR = "F_TFE"
  )
)
} # }
```

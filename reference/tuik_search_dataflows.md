# Search TÜİK SDMX Dataflows

Searches the TÜİK SDMX Dataflow catalogue using a keyword or phrase.

## Usage

``` r
tuik_search_dataflows(
  query,
  language = c("both", "tr", "en"),
  final_only = FALSE,
  limit = 25,
  fuzzy = TRUE,
  refresh = FALSE,
  dataflows = NULL,
  api_key = NULL
)
```

## Arguments

- query:

  Character string containing the search term.

- language:

  Which label language should be searched. One of "both", "tr", or "en".
  Defaults to "both".

- final_only:

  Logical. If TRUE, return only final Dataflows.

- limit:

  Maximum number of results. Use NULL for all matches.

- fuzzy:

  Logical. If TRUE, perform fallback matching when the exact search
  returns no results.

- refresh:

  Logical. If TRUE, refresh the Dataflow catalogue.

- dataflows:

  Optional Dataflow catalogue supplied by the user.

- api_key:

  Optional API key. If NULL, the TUIK_SDMX_API_KEY environment variable
  is used when retrieving the catalogue.

## Value

A tibble containing matching Dataflows.

## Details

Search fields:

- Dataflow ID

- Turkish Dataflow name

- English Dataflow name

- Referenced DSD ID

Search is case-insensitive and tolerant of Turkish characters.

For example:

"tüketici fiyat" "tuketici fiyat"

are treated equivalently.

If no exact substring match is found, an optional conservative fallback
search is performed using shortened word variants. This may help with
expressions such as "işsizlik" when catalogue labels contain "işsiz".

## Examples

``` r
if (FALSE) { # \dontrun{
tuik_search_dataflows("tüketici fiyat")

tuik_search_dataflows("tuketici fiyat")

tuik_search_dataflows("işsizlik")

tuik_search_dataflows(
  "consumer price",
  language = "en"
)
} # }
```

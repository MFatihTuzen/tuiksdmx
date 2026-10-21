# Create fallback search variants

Generates progressively shorter variants of a search term.

## Usage

``` r
tuik_search_variants(x, max_trim = 3)
```

## Arguments

- x:

  Single character string.

- max_trim:

  Maximum number of trailing characters to remove.

## Value

Character vector of candidate search strings.

## Details

This is used only when an exact substring search returns no results. It
provides a lightweight fallback for cases such as:

"issizlik" -\> "issiz"

It is deliberately conservative and is not intended to be a linguistic
stemmer.

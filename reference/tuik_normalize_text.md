# Normalise text for searching

Converts text to lower case, replaces Turkish-specific characters with
ASCII equivalents and normalises whitespace.

## Usage

``` r
tuik_normalize_text(x)
```

## Arguments

- x:

  Character vector.

## Value

A normalised character vector.

## Details

Examples: "Tuketici Fiyat" -\> "tuketici fiyat" "ISGUCU" -\> "isgucu"

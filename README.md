# dialtone

<!-- badges: start -->
[![R-CMD-check](https://github.com/gojiplus/dialtone/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/gojiplus/dialtone/actions/workflows/R-CMD-check.yml)
<!-- badges: end -->

Is this string a valid phone number, what kind is it, which country does it
belong to, and how should it be formatted? `dialtone` answers for a whole
character vector at once across all 245 CLDR regions and 9 non-geographic
calling codes, using Google's `libphonenumber` metadata (`v9.0.43`) with no
Java dependency and no runtime downloader. Malformed values return `FALSE` or
`NA` rather than stopping.

A `TRUE` from `phone_valid()` means the number is well-formed under that
country's numbering plan. It does not mean the number is assigned or active.

## Installation

From CRAN:

```r
install.packages("dialtone")
```

The development version from GitHub:

```r
# install.packages("pak")
pak::pak("gojiplus/dialtone")
```

## Parsing, validation and formatting

```r
library(dialtone)

x <- c("+1 650-253-0000", "+44 20 7946 0192", "+1 111-111-1111", "not a number", NA)

# Possible checks length only; valid checks the country's numbering plan
phone_possible(x)
#> [1]  TRUE  TRUE  TRUE FALSE    NA
phone_valid(x)
#> [1]  TRUE  TRUE FALSE FALSE    NA

phone_region(x)
#> [1] "US" "GB" NA   NA   NA
phone_type(x)
#> [1] "fixed_line_or_mobile" "fixed_line"           "unknown"              NA                     NA

phone_format(x, format = "E164")
#> [1] "+16502530000" "+442079460192" "+11111111111" NA             NA
phone_format(x, format = "NATIONAL")
#> [1] "(650) 253-0000" "020 7946 0192"  "1111111111"     NA               NA
```

Numbers written in national format require a default `region`:

```r
phone_valid(c("020 7946 0192", "07700 900077", "020 0000 0000"), region = "GB")
#> [1]  TRUE  TRUE FALSE

phone_type(c("020 7946 0192", "07700 900077"), region = "GB")
#> [1] "fixed_line" "mobile"
```

When `strict = TRUE`, `phone_valid()` also checks that the number belongs to
`region` (for example, distinguishing US and Canadian numbers under `+1`):

```r
phone_valid(c("+1 650-253-0000", "+1 416-555-0199"), region = "US", strict = FALSE)
#> [1] TRUE TRUE
phone_valid(c("+1 650-253-0000", "+1 416-555-0199"), region = "US", strict = TRUE)
#> [1]  TRUE FALSE
```

`phone_parse()` returns all components at once in a data frame that can also be
passed directly to `phone_valid()`, `phone_type()`, `phone_region()` and
`phone_format()` without re-parsing:

```r
phone_parse(c("+39 06 6981", "011 15-2345-6789 ext. 88"), region = "AR")
#>   country_code national_number extension region possible valid       type          e164
#> 1           39          066981      <NA>     IT     TRUE  TRUE fixed_line    +39066981
#> 2           54     91123456789        88     AR     TRUE  TRUE     mobile +5491123456789
```

`phone_regions()` lists the 254 supported territories, their country calling
codes, national prefixes and upstream example numbers:

```r
head(phone_regions()[, c("region", "country_code", "main_country", "example_fixed_line")])
#>   region country_code main_country example_fixed_line
#> 1     AC          247         TRUE              62000
#> 2     AD          376         TRUE             712345
#> 3     AE          971         TRUE          223456789
#> 4     AF           93         TRUE          234567890
#> 5     AG            1        FALSE         2684621234
#> 6     AI            1        FALSE         2644611234
```

## What it rests on

[Rules and their sources](https://gojiplus.github.io/dialtone/articles/rules.html)
describes the prior art on CRAN, how Google's `libphonenumber` (`v9.0.43`)
metadata and parsing rules are compiled into `dialtone`, and how parity with the
Python `phonenumbers` port is verified across all 1,141 upstream example
numbers, a hand-written corpus of awkward edge cases, and 20,000 random and
perturbed inputs.

## Related packages

- [`dialr`](https://cran.r-project.org/package=dialr) wraps the Java
  `libphonenumber` `.jar` through `rJava`.
- [`idcheck`](https://github.com/gojiplus/idcheck) validates identity numbers
  and check digits.
- [`piinata`](https://github.com/gojiplus/piinata) scans research files for
  personal data, including phone numbers inside running text.

## License

Apache License (>= 2). Contains code and metadata derived from Google's
[`libphonenumber`](https://github.com/google/libphonenumber) (`v9.0.43`), also
under the Apache License 2.0; see `inst/COPYRIGHTS`.

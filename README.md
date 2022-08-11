
# rndlsearch

Query the [Search API](https://iss.ndl.go.jp/information/outline/) of
National Diet Library.

## Installation

``` r
library(devtools)
install_github("CORDEA/rndlsearch")
```

## Example

``` r
library(rndlsearch)
results <- rndlsearch(
  limit = 2,
  title = "Shakespeare",
  from = "2000",
  until = "2020"
)
```

# Converts a data product to a link

Given the name and version of a data product, this will return a custom
function for retrieving that data product

## Usage

``` r
dp_tolink(dp_name, dp_version)
```

## Arguments

- dp_name:

  name of the data product

- dp_version:

  version of the data product

## Examples

``` r
if (FALSE) { # \dontrun{
dp_cars_lnk <- dp_tolink(dp_name = "dp-cars-us001", dp_version = "4dc379a")
} # }
```

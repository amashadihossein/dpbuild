# Hydrate a dried called function

execute and returns the value of function call given its textual (dried)
representation

## Usage

``` r
fn_hydrate(dried_fn)
```

## Arguments

- dried_fn:

  a function called

## Value

value of the called function given its textual representation

## Examples

``` r
if (FALSE) { # \dontrun{
fn_hydrate(fn_dry(sum(log(1:10))))
} # }
```

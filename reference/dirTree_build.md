# Build directory tree

Receives a flattened directory tree from `dirTree_flatten` and appends a
metadata tibble to each element

## Usage

``` r
dirTree_build(flattened_dirTree)
```

## Arguments

- flattened_dirTree:

  out of call to `dirTree_flatten`

## Value

dirTree a similar to output of `dirTree_flatten` but with each element
enriched with tidy metadata including sha1 and original path to each
data

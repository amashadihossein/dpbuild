# Clean input_map

This function drops unsynced inputs from the input map and cleans names

## Usage

``` r
inputmap_clean(input_map, remove_id = character(0), force_cleanname = F)
```

## Arguments

- input_map:

  synced mapped object as returned by
  [`dpbuild::dpinput_map`](https://amashadihossein.github.io/dpbuild/reference/dpinput_map.md)

- remove_id:

  a vector of input_data ids to remove. This is for convenience as
  setting the input_manifest field `to_be_synced` to FALSE can achieve
  the same thing. The default value of `character(0)` limits removal to
  any row with `to_be_synced == FALSE`

- force_cleanname:

  T/F, if TRUE it ensures each input id name ends up being unique. If
  FALSE, it won't clean names unless names are already unique

## Value

input_map pruned and with cleaner names

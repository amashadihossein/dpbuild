# Reset sync flag in the manifest

It updates input_map manifest and reverses the to_be_synced flag state

## Usage

``` r
dpinput_syncflag_reset(input_map, input_id)
```

## Arguments

- input_map:

  input_map made with `dpinput_map`

- input_id:

  a vector of character strings matching `input_mapt$input_manifest$id`

## Value

modified `input_map`

# Write data product input manifest

Converts structured input into a yaml manifest and writes to
`.daap/daap_input.yaml`

## Usage

``` r
dpinput_write(project_path, input_d, verbose = F)
```

## Arguments

- project_path:

  Project path

- input_d:

  a list generated from `dpinput_sync` with synced_input elements
  structured as desired

- verbose:

  T/F

## Value

TRUE

# Read data product input manifest

Reads yaml dpinput manifest and returns a structured pinlink

## Usage

``` r
dpinput_read(
  daap_input_yaml = yaml::read_yaml(file = "./.daap/daap_input.yaml"),
  add_metadata = F
)
```

## Arguments

- daap_input_yaml:

  daap_input yaml file imported (default "./.daap/daap_input.yaml")

- add_metadata:

  T/F when TRUE dpinput format will include metadata

## Value

daap_input as a structured list of anonymous functions each when called
retrieves the specific data

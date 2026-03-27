# Get data product config information

Reads in data product config file `.daap/daap_config.yaml`and returns
config details

## Usage

``` r
dpconf_get(project_path = ".")
```

## Arguments

- project_path:

  path to project folder

## Value

a list dpconf

## Details

This function reads in the yaml config as a list. In the process, it
hydrates any expression for `board_params` and `creds`. Make sure
environment variables declared in dried functions are set prior to
calling `dpconf_get`.

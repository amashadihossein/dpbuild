# Get Pins Version Pre Deploy

This get the pins version pre-deploy

## Usage

``` r
get_pin_version(d, pin_name, pin_description, type = "rds")
```

## Arguments

- d:

  data object

- pin_name:

  what the pin will be named. For data products, it is encoded in
  dp_param

- pin_description:

  what the pin description will be. For data products, it is encoded in
  dp_params

- type:

  File type used to save the data product, default RDS

## Value

a character version

# Write data product

This function writes the data product and logs it in
`.daap/daap_log.yaml`

## Usage

``` r
dp_write(data_object, type = "rds", project_path = ".")
```

## Arguments

- data_object:

  data_object generated from `dp_structure`

- type:

  File type used to save the data product, default RDS

- project_path:

  path to the project (default is current directory)

## Value

TRUE

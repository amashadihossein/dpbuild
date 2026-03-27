# Process a zip directory

This function simply extends the functionality of `dir_process` to
zipped directories. it unzips in temp directory and once unzipped calls
`dir_process`

## Usage

``` r
dir_process_zip(zip_dir)
```

## Arguments

- zip_dir:

  path to the zipped directory

## Value

read_files a list of read contents

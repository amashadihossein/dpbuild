# Process current input directory

Recursively reads and hashes the content of the current directory
returning the content as a list that matches the structure of the
directory

## Usage

``` r
dir_process(current_dir, junk_path = character(0))
```

## Arguments

- current_dir:

  path to the current directory

- junk_path:

  path to be dropped from the path prefix it prevents repeating the
  capture of folder structure already captured

## Value

read_files a list of read contents

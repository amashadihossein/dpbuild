# Push data product to remote repo

This command pushes the data product to the remote

## Usage

``` r
dp_push(
  project_path = ".",
  remote_alias = character(0),
  remote_url = character(0)
)
```

## Arguments

- project_path:

  path to the dp_project (default is current directory)

- remote_alias:

  use only to overwrite default alias, or if no default remote alias has
  been configured (e.g. legacy).

- remote_url:

  use only to overwrite default URL, or if no default remote URL has
  been configured (e.g. legacy).

## Value

TRUE

## Examples

``` r
if (FALSE) { # \dontrun{
dp_push(project_path = ".")
} # }
```

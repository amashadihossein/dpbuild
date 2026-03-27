# Pull data product from a remote repo

This command pulls the data product from a remote repo

## Usage

``` r
dp_pull(project_path = fs::path_wd())
```

## Arguments

- project_path:

  Path to the project folder (default is working directory)

## Value

TRUE

## Examples

``` r
 if (FALSE) { # \dontrun{
Sys.setenv("GITHUB_PAT" = keyring::key_get("GITHUB_PAT"))
dp_pull()
} # }
```

# Commit data product

Runs a number of checks and commits data product changes

## Usage

``` r
dp_commit(project_path = fs::path_wd(), commit_description)
```

## Arguments

- project_path:

  Path to the project folder (default is working directory)

- commit_description:

  Commit message

## Value

repo object post commit

## Examples

``` r
if (FALSE) { # \dontrun{
dp_commit(commit_description = "First dp commit")
} # }
```

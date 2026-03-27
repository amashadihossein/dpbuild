# Clone data product project from a remote repo

This command clones data product project from a remote repo

## Usage

``` r
dp_clone(remote_url, branch, verbose = F)
```

## Arguments

- remote_url:

  a url like `https://github.com/teamAccount/me/dp_test1.git`

- branch:

  branch name of the data product (same as that of repo)

- verbose:

  T/F if TRUE, it'll show cloning progress

## Value

TRUE

## Examples

``` r
if (FALSE) { # \dontrun{
Sys.setenv("GITHUB_PAT" = keyring::key_get("GITHUB_PAT"))
dp_clone(
  remote_url = "https://github.com/teamAccount/me/dp_test1.git",
  branch = us001
)
} # }
```

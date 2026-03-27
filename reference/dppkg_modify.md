# R package version update

Modifies R package records of `renv` for this project

## Usage

``` r
dppkg_modify(
  project_path = ".",
  pkg_name,
  pkg_version = character(0),
  pkg_sha = character(0),
  repo_name = "RSPM",
  repo_url,
  force_repo_overwrite = F,
  verbose = T
)
```

## Arguments

- project_path:

  path to the project

- pkg_name:

  name of the package to be added or updated

- pkg_version:

  pkg version, if not provided, tries to retrieve the latest from RSPM

- pkg_sha:

  sha as recorded on the repository. tries to retrieve the latest from
  RSPM

- repo_name:

  by default RSPM

- repo_url:

  url corresponding to the repo where the package is

- force_repo_overwrite:

  T/F. Use with caution. Changing this, will change the url
  corresponding to the repo name/alias universally and may break the
  lock file

- verbose:

  T/F

## Value

the update renv.lock JSON as a list

## Details

This is primarily intended to enable updating package that are not on
CRAN or GitHub (e.g. RSPM packages).
[`renv::update`](https://rstudio.github.io/renv/reference/update.html)
can update when packages are on CRAN or GitHub. However as core packages
of `DaaPR` are internal, this helper function is currently needed. Note,
currently update to packages from git are not supported. To update DaaPR
elements, set install options to RSPM `options(repos = <RSPM URL>)`
Update DaaPR \> run this function to update all DaaPR packages on
renv.lock \> Delete `renv` folder \> run
[`renv::restore()`](https://rstudio.github.io/renv/reference/restore.html)
\> Follow subsequent steps.

## Examples

``` r
if (FALSE) { # \dontrun{
dppkg_modify(
  project_path = ".", pkg_name = "dpdeploy",
  pkg_version = "0.0.0.9008",
  pkg_sha = "3e9544b7ea42f683647df5a4e21238ae7194b580", verbose = T
)
} # }
```

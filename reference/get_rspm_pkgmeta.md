# R the latest package metadata on rspm

A convenience function to hit the rspm api and get pkg metadata

## Usage

``` r
get_rspm_pkgmeta(pkg_name, rspm_api_url)
```

## Arguments

- pkg_name:

  character string name of the package

- rspm_api_url:

  the api endpoint of rspm that given pkg_name retrieves pkg metadata
  like latest version, remote_sha, etc.

## Value

pkgmeta containing `name`, `remote_sha`, `version`, `checksum`, and
`repository`

## Details

This is primarily intended to make `dppkg_modify` easier to work with

## Examples

``` r
if (FALSE) { # \dontrun{
get_rspm_pkgmeta(pkg_name = "dpi")
} # }
```

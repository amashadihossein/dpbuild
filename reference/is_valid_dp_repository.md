# Determine if valid dp repository

Looks at the path, runs `dp_repository_check` and returns TRUE if all
TRUE

## Usage

``` r
is_valid_dp_repository(
  path,
  checks = c("all", "git", "dp", "renv", "branch"),
  verbose = F
)
```

## Arguments

- path:

  Path to be evaluated

- checks:

  any combination of c("all","git","dp","renv","branch"). default is
  all.

- verbose:

  If TRUE, it will print which tests passed/failed

## Value

TRUE or FALSE

## Details

All diagnostic tests to check validity of dp repository are run
regardless of choice of checks. Checks determines what subset is
considered in return T/F.

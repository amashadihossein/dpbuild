# Add readme to the project

Add readme to the project

## Usage

``` r
add_readme(
  project_path,
  dp_title,
  github_repo_url,
  board_params_set_dried,
  creds_set_dried
)
```

## Arguments

- project_path:

  Path to the project folder

- dp_title:

  readme title

- github_repo_url:

  github repo url

- board_params_set_dried:

  Character representation of the function for setting board_params. Use
  [`fn_dry()`](https://amashadihossein.github.io/dpbuild/reference/fn_dry.md)
  in combination with
  [`board_params_set_s3()`](https://rdrr.io/pkg/dpi/man/board_params_set_s3.html),
  [`board_params_set_labkey()`](https://rdrr.io/pkg/dpi/man/board_params_set_labkey.html),
  or
  [`board_params_set_local()`](https://rdrr.io/pkg/dpi/man/board_params_set_local.html).

- creds_set_dried:

  Character representation of the function for setting creds. Use
  [`fn_dry()`](https://amashadihossein.github.io/dpbuild/reference/fn_dry.md)
  in combination with
  [`creds_set_aws()`](https://rdrr.io/pkg/dpi/man/creds_set_aws.html) or
  [`creds_set_labkey()`](https://rdrr.io/pkg/dpi/man/creds_set_labkey.html).

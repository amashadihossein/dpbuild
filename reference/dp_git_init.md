# dp git initialization

Initializes the git repo according to the requirements of dp

## Usage

``` r
dp_git_init(
  project_path,
  project_name,
  branch_name,
  github_repo_url,
  board_params_set_dried,
  creds_set_dried,
  git_ignore
)
```

## Arguments

- project_path:

  Path to the project folder

- project_name:

  The name of the project. This is typically the name of the folder
  where the project is set

- branch_name:

  An abbreviation to capture the specific reason for which data was
  processed. Example m3cut (as in month 3 data cut)

- github_repo_url:

  the https url for the github repo

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

- git_ignore:

  A character vector of the files and directories to be ignored by git.

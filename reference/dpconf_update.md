# Modify data product configuration

Manages all modification to data product project configuration
subsequent to the initialization via `dp_init`.

## Usage

``` r
dpconf_update(
  project_path = fs::path_wd(),
  project_description = character(0),
  branch_name = character(0),
  branch_description = character(0),
  readme_general_note = character(0),
  board_params_set_dried = character(0),
  creds_set_dried = character(0),
  commit_description = "dp_conf modified",
  git_ignore = character(0),
  ...
)
```

## Arguments

- project_path:

  path to the project folder. The folder name will be used as project
  name. If the path doesn't exist it will be created.

- project_description:

  A high level description of the project. Example: integrated, clinical
  and translational data from study x.

- branch_name:

  An abbreviation to capture the specific reason for which data was
  processed. Example m3cut (as in month 3 data cut)

- branch_description:

  A high level description of the branch

- readme_general_note:

  Optional general note which will be added as metadata to the data
  object

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
  in combination with `creds_set_aws` or
  [`creds_set_labkey()`](https://rdrr.io/pkg/dpi/man/creds_set_labkey.html).

- commit_description:

  A character string that describes what was updated.

- git_ignore:

  a character vector of the files and directories to be added to
  existing list ignored by git.

- ...:

  any other metadata to be captured in the config file

## Value

project path

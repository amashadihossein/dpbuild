# Initialize data product project

Initializes a data product project and creates local data product folder

## Usage

``` r
dp_init(
  project_path = fs::path_wd(),
  project_description,
  branch_name,
  branch_description,
  readme_general_note = character(0),
  board_params_set_dried,
  creds_set_dried,
  github_repo_url,
  git_ignore = c(".drake/", "_targets/", "*_files/", ".Rprofile", ".Renviron",
    ".Rhistory", ".Rapp.history", ".Rproj.user", ".Rproj.user/", ".DS_Store", "*.csv",
    "*.tsv", "*.rds", "*.txt", "*.parquet", "*.sas7bdat", ".RData", ".RDataTmp",
    "*.html", "*.png", "*.pdf", ".vscode/", "rsconnect/", "*_cache/"),
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

  when using `local_board`, it is ignored and need not be specified.
  Otherwise,character representation of the function for setting creds.
  Use
  [`fn_dry()`](https://amashadihossein.github.io/dpbuild/reference/fn_dry.md)
  in combination with `creds_set_aws` or `creds_set_labkey`. *NOTE:
  never directly pass credentials in script!* *Use
  [`Sys.getenv()`](https://rdrr.io/r/base/Sys.getenv.html)*. See example

- github_repo_url:

  the https url for the github repo

- git_ignore:

  a character vector of the files and directories to be ignored by git.

- ...:

  any other metadata to be captured in the config file

## Value

project path

## Details

This function will create a local git-tracked data product repository,
write the data product configuration details to
`.daap/daap_config.yaml`, initialize renv for the project, and commit
all changes.

## Examples

``` r
if (FALSE) { # \dontrun{
# Dry function call to setting board_params
board_params_set_dried <-
  fn_dry(dpi::board_params_set_s3(
    bucket_name = "bucket_name",
    region = "us-east-1"
  ))

# Dry function call to setting credentials
creds_set_dried <-
  fn_dry(dpi::creds_set_aws(
    key = Sys.getenv("AWS_KEY"),
    secret = Sys.getenv("AWS_SECRET")
  ))

# Initialize dp repo
dp_repo <- dp_init(
  project_path = "dp_test",
  project_description = "Test data product",
  branch_name = "us001",
  branch_description = "User story 1",
  readme_general_note = "This data object is generated for testing purposes",
  board_params_set_dried = board_params_set_dried,
  creds_set_dried = creds_set_dried,
  github_repo_url = "https://github.com/teamAccount/me/dp_test.git"
)
} # }
```

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dp_init.R
\name{dp_init}
\alias{dp_init}
\title{Initialize data product project}
\usage{
dp_init(
  project_path = fs::path_wd(),
  project_description,
  branch_name,
  branch_description,
  readme_general_note = character(0),
  board_params_set_dried,
  creds_set_dried,
  github_repo_url,
  git_ignore = c(".drake/", "input_files/", "output_files/", ".Rhistory", ".DS_Store"),
  ...
)
}
\arguments{
\item{project_path}{path to the project folder. The folder name will be used
as project name. If the path doesn't exist it will be created.}

\item{project_description}{A high level description of the project. Example:
integrated, clinical and translational data from study x.}

\item{branch_name}{An abbreviation to capture the specific reason for which
data was processed. Example m3cut (as in month 3 data cut)}

\item{branch_description}{A high level description of the branch}

\item{readme_general_note}{Optional general note which will be added as
metadata to the data object}

\item{board_params_set_dried}{Character representation of the function for
setting board_params. Use \code{fn_dry()} in combination with
\code{dpi::board_params_set_s3} or \code{dpi::board_params_set_labkey}. See example.}

\item{creds_set_dried}{Character representation of the function for setting
creds. Use \code{fn_dry()} in combination with \code{dpi::creds_set_aws} or
\code{dpi::creds_set_labkey}. See example}

\item{github_repo_url}{the https url for the github repo}

\item{git_ignore}{a character vector of the files and directories to be
ignored by git.}

\item{...}{any other metadata to be captured in the config file}
}
\value{
project path
}
\description{
Initializes a data product project
}
\examples{
 \dontrun{
# Dry function call to setting board_params
board_params_set_dried <- 
fn_dry(dpi::board_params_set_labkey(board_alias = "labkey",
 url = 'https:url_to_labkey/labkey',
 folder = 'project_folder/subfolder'))

# Dry function call to setting credentials
creds_set_dried <- 
fn_dry(dpi::creds_set_labkey(api_key = Sys.getenv("LABKEY_API_KEY")))

# Initialize dp repo
dp_repo <- dp_init(project_path = "dp_test",
                project_description = "Test data product",
                branch_name = "us001",
                branch_description = "User story 1",
                commit_description = "First dp commit",
                readme_general_note = "This data object is generated for testing purposes",
                board_params_set_dried = board_params_set_dried,
                creds_set_dried = creds_set_dried,
                github_repo_url = "https://github.com/teamAccount/me/dp_test.git")
}
}

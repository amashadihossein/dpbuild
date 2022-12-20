#' @title Pull data product from a remote repo
#' @description This command pulls the data product from the remote
#' @param project_path path to the dp_project
#' @return TRUE
#' @examples  \dontrun{
#' Sys.setenv("GITHUB_PAT" = keyring::key_get("GITHUB_PAT"))
#' dp_pull(project_path = ".")
#' }
#' @export

dp_pull <- function (project_path = ".")
{
  dirs_to_add <- c("input_files", "output_files")

  if(nchar(Sys.getenv("GITHUB_PAT")) == 0)
    stop(cli::format_error(glue::glue("Could not find ",
      "GITHUB_PAT in the environment. Set it ",
      "by Sys.setenv(GITHUB_PAT = <your github",
      " personal access token>) and retry!")))
  cred <- git2r::cred_token()

  if(fs::dir_exists(project_path)){
    if(length(fs::dir_ls(project_path)) <= 0)
      stop(cli::format_error("{project_path} is an empty directory!"))
  }

  if (!dpbuild::is_valid_dp_repository(project_path)){
    stop(cli::format_error("dp_pull failed; make sure this is a valid git repository."))
  }

  repo <- tryCatch({
    git2r::pull(repo = project_path, credentials = cred)

    fs::dir_create(dirs_to_add[!fs::dir_exists(dirs_to_add)])
  },
    error = function(cond) {
      message("Encountered error in dp_pull")
      message("Make sure GITHUB_PAT is correct")
      message(glue::glue("Make sure {project_path} directory exists"))
      message("Networking constraints (e.g. vpn) may be blocking communication")
      message(cond)
    }
  )
  return(TRUE)
}

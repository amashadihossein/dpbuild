#' @title Pull data product from a remote repo
#' @description This command pulls the data product from a remote repo
#' @param project_path Path to the project folder, default "."
#' @return TRUE
#' @examples  \dontrun{
#' Sys.setenv("GITHUB_PAT" = keyring::key_get("GITHUB_PAT"))
#' dp_pull()
#' @export
dp_pull <- function(project_path = fs::path_wd())
{
  dirs_to_add <- c("input_files", "output_files")

  if(nchar(Sys.getenv("GITHUB_PAT")) == 0)
    stop(cli::format_error(glue::glue("Could not find ",
      "GITHUB_PAT in the environment. Set it ",
      "by Sys.setenv(GITHUB_PAT = <your github",
      " personal access token>) and retry!")))
  cred <- git2r::cred_token()

  if (!dpbuild::is_valid_dp_repository(project_path)){
    stop(cli::format_error("dp_pull failed; make sure this is a valid dp git repository."))
  }

  repo <- tryCatch({
    git2r::pull(repo = project_path, credentials = cred)

    fs::dir_create(dirs_to_add[!fs::dir_exists(fs::path(project_path, dirs_to_add))])
  },
    error = function(cond) {
      cli::cli_alert_danger("Encountered error in dp_pull")
      cli::cli_alert_warning("Make sure GITHUB_PAT is correct")
      cli::cli_alert_warning("Networking constraints (e.g. vpn) may be blocking communication")
      cli::cli_alert_danger(cond)
    }
  )
  return(TRUE)
}

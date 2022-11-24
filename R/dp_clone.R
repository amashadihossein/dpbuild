#' @title Clone data product project from a remote repo
#' @description This command clones data product project from a remote repo
#' @param remote_url a url like `https://github.com/teamAccount/me/dp_test1.git`
#' @param branch branch name of the data product (same as that of repo)
#' @param verbose T/F if TRUE, it'll show cloning progress
#' @return TRUE
#' @examples  \dontrun{
#' Sys.setenv("GITHUB_PAT" = keyring::key_get("GITHUB_PAT"))
#' dp_clone(remote_url = "https://github.com/teamAccount/me/dp_test1.git",
#' branch = us001)
#' }
#' @export

dp_clone <- function(remote_url, branch,  verbose = F){

  input_output_directories <- c("input_files", "output_files")

  if(nchar(Sys.getenv("GITHUB_PAT")) == 0)
    stop(cli::format_error(glue::glue("Could not push as it did not find ",
                                      "GITHUB_PAT in the environment. Set it ",
                                      "by Sys.setenv(GITHUB_PAT = <your github",
                                      " personal access token>) and retry!")))
  cred <- git2r::cred_token()
  project_path <- fs::path(fs::path_wd(),
                     fs::path_ext_remove(fs::path_file(remote_url)))

  if(fs::dir_exists(project_path)){
    if(length(fs::dir_ls(project_path)) > 0)
      stop(cli::format_error("{project_path} is not an empty directory!"))
  }


  repo <- tryCatch({
    git2r::clone(url = remote_url, local_path = project_path, branch = branch,
                 credentials = cred, progress = verbose)

    for (dir in input_output_directories){
      if(!fs::dir_exists(dir)){
        fs::dir_create(dir)
      }
    }
  },
  error = function(cond) {
    message("Encountered error in dp_clone")
    message("Make sure GITHUB_PAT is correct")
    message(glue::glue("Make sure remote url {remote_url} exists"))
    message(glue::glue("Make sure branch {branch} exists"))
    message("Networking constraints (e.g. vpn) may be blocking communication")
    message(cond)
  }
  )

  return(TRUE)
}

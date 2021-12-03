#' @title Push data product to remote repo
#' @description This command pushes the data product to the remote
#' @param project_path path to the dp_project
#' @param remote_alias use only to overwrite default alias, or if no default
#' remote alias has been configured (e.g. legacy).
#' @param remote_url use only to overwrite default URL, or if no default remote
#' URL has been configured (e.g. legacy).
#' @return TRUE
#' @examples \dontrun{
#' dp_push(project_path = ".")
#' }
#' @export

dp_push <- function(project_path = ".",
                    remote_alias = character(0),
                    remote_url = character(0)) {
  if (nchar(Sys.getenv("GITHUB_PAT")) == 0) {
    stop(cli::format_error(glue::glue(
      "Could not push as it did not find ",
      "GITHUB_PAT in the environment. Set it ",
      "by Sys.setenv(GITHUB_PAT = <your github",
      " personal access token>) and retry!"
    )))
  }

  repo <- git2r::repository(path = project_path)
  cred <- git2r::cred_token()

  if (length(remote_url) == 0) {
    remote_url <- git2r::remote_url(repo = repo)
    if (length(remote_url) == 0) {
      stop(cli::format_error(glue:glue(
        "remote url is not configured! Add ",
        "provide remote url parameter"
      )))
    }
  }

  if (length(remote_alias) == 0) {
    remote_alias <- git2r::remotes(repo = repo)
    if (length(remote_alias) == 0) {
      remote_alias <- "origin"
    }
  } else {
    if (!remote_alias %in% git2r::remotes()) {
      git2r::remote_add(repo = repo, name = remote_alias, url = remote_url)
    }
  }

  # TODO: evaluate behavior if there are multiple remotes

  current_branch <- git2r::repository_head(repo)

  tryCatch(
    {
      git2r::push(
        object = repo, name = remote_alias,
        refspec = glue::glue("refs/heads/{current_branch$name}"),
        set_upstream = TRUE, credentials = cred
      )
    },
    error = function(cond) {
      message("Encountered error in dp_push")
      message("Make sure GITHUB_PAT is correct")
      message(glue::glue("Make sure {remote_url} is created"))
      message("Networking constraints (e.g. vpn) may be blocking communication")
      message(cond)
    }
  )

  return(TRUE)
}

#' @title Modify data product configuration
#' @description Manages all mocification to data product project configuration
#' subsequent to the initialization via `dp_init`.
#' @param project_path path to the project folder. The folder name will be used
#' as project name. If the path doesn't exist it will be created.
#' @param project_description A high level description of the project. Example:
#' integrated, clinical and translational data from study x.
#' @param branch_name An abbreviation to capture the specific reason for which
#' data was processed. Example m3cut (as in month 3 data cut)
#' @param branch_description A high level description of the branch
#' @param readme_general_note Optional general note which will be added as
#' metadata to the data object
#' @param board_params_set_dried Character representation of the function for
#' setting board_params. Use `fn_dry()`
#' in combination with `dpi::board_params_set_s3` or
#' `dpi::board_params_set_labkey`. See example.
#' @param creds_set_dried Character representation of the function for setting
#' creds. Use `fn_dry()` in combination with
#' `dpi::creds_set_aws` or `dpi::creds_set_labkey`. See example
#' @param commit_description A character string that describes what was updated.
#' @param git_ignore a character vector of the files and directories to be added
#'  to existing list ignored by git.
#' @param ... any other metadata to be captured in the config file
#' @return project path
#' @export


dpconf_update <- function(project_path = fs::path_wd(),
                          project_description = character(0),
                          branch_name = character(0),
                          branch_description = character(0),
                          readme_general_note = character(0),
                          board_params_set_dried = character(0),
                          creds_set_dried = character(0),
                          commit_description = "dp_conf modified",
                          git_ignore = character(0),
                          ...) {
  if (!is_valid_dp_repository(path = project_path)) {
    message("Invalid, incomplete or incosistent dp repository:")
    ck <- dp_repository_check(path = project_path)
    ck_formatted <- sapply(names(ck), function(ck_i) {
      if (ck[[ck_i]]) {
        return(cli::cli_alert_success(text = glue::glue("{ck_i}:{ck[[ck_i]]}")))
      }
      return(cli::cli_alert_danger(text = glue::glue("{ck_i}:{ck[[ck_i]]}")))
    }, simplify = F)

    stop(cli::format_error(glue::glue(
      "Not a valid dp repository. Ensure this ",
      "function is run on a project initiated ",
      "via dp_init"
    )))
  }

  dpconf <- dpconf_read(project_path = project_path)
  repo <- git2r::repository(path = project_path)

  add_gitignore <- setdiff(git_ignore, readLines(file.path(
    project_path,
    ".gitignore"
  )))
  if (length(add_gitignore) > 0) {
    writeLines(
      glue::glue_collapse(
        {
          c(git_ignore, add_gitignore)
        },
        sep = "\n"
      ),
      file.path(project_path, ".gitignore")
    )
  }

  if (length(project_description) > 0) {
    dpconf$project_description <- project_description
  }

  if (length(branch_name) > 0) {
    # Create a branch
    branch_1 <- git2r::branch_create(
      commit = git2r::last_commit(repo = repo),
      name = branch_name
    )

    # change branch
    git2r::checkout(object = repo, branch = branch_name)
    dpconf$branch_name <- branch_name
  }


  if (length(branch_description) > 0) {
    dpconf$branch_description <- branch_description
  }

  if (length(readme_general_note) > 0) {
    dpconf$readme_general_note <- readme_general_note
  }

  if (length(board_params_set_dried) > 0) {
    dpconf$board_params_set_dried <- board_params_set_dried
  }

  if (length(creds_set_dried) > 0) {
    dpconf$creds_set_dried <- creds_set_dried
  }

  dpconf_write(project_path = project_path, dpconf = dpconf)

  tryCatch({
    project_name <- basename(path = project_path)
    add_readme(
      project_path = project_path,
      dp_title = glue::glue("Data Product {project_name}_{branch_name}"),
      github_repo_url = github_repo_url,
      board_params_set_dried = board_params_set_dried,
      creds_set_dried = creds_set_dried
    )
  },
    error = function(cond) {
      cli::cli_alert_danger("Encountered error in dpconf_update.
                             Make sure parameters are correctly passed.
                             For example, board params need to be dried.")
      cli::cli_alert_danger(cond)
    }
  )

  add_these <- unlist(git2r::status(repo = repo))
  git2r::add(repo = repo, path = glue::glue("{project_path}/{add_these}"))
  git2r::commit(repo = repo, all = TRUE, message = commit_description)

  return(fs::path_dir(repo$path))
}

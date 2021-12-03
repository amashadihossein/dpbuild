#' @title Adds script templates
#' @description Adds dp_assembler.R and dp_structure.R to the dp project repo
#' @param project_path Path to the Project
#' @return repo
#' @export
dpcode_add <- function(project_path) {
  if (!fs::dir_exists(project_path)) {
    stop("project_path does not exist")
  }

  if (!is_valid_dp_repository(path = project_path)) {
    stop(cli::format_error(glue::glue(
      "Not a dp repsitory. Run ",
      "`dp_repository_check(\"{project_path}\")`",
      " for details"
    )))
  }

  if (!fs::dir_exists(fs::path_tidy(glue::glue("{project_path}/R")))) {
    fs::dir_create(fs::path_tidy(glue::glue("{project_path}/R")))
  }

  fs::file_copy(
    path = system.file("dp_journal.RMD", package = "dpbuild"),
    new_path = project_path
  )
  fs::file_copy(
    path = system.file("dp_make.R", package = "dpbuild"),
    new_path = project_path
  )

  # commit
  repo <- git2r::repository(path = project_path)
  git2r::add(
    repo = repo, path =
      fs::path_tidy(glue::glue("{project_path}/dp_make.R"))
  )
  git2r::add(
    repo = repo, path =
      fs::path_tidy(glue::glue("{project_path}/dp_journal.RMD"))
  )
  repo <- git2r::commit(
    repo = repo,
    message = "Added template code to dp project"
  )
  return(repo)
}

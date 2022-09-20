#' @title Adds script templates
#' @description Adds dp_assembler.R and dp_structure.R to the dp project repo
#' @param project_path Path to the Project
#' @param use_targets Boolean When true, it uses targets instead of drake
#' @return repo
#' @export
dpcode_add <- function(project_path, use_targets=F){

  if(!fs::dir_exists(project_path))
    stop("project_path does not exist")

  if(!is_valid_dp_repository(path = project_path))
    stop(cli::format_error(glue::glue("Not a dp repsitory. Run ",
                                      "`dp_repository_check(\"{project_path}\")`",
                                      " for details")))

  if(!fs::dir_exists(fs::path_tidy(glue::glue("{project_path}/R"))))
    fs::dir_create(fs::path_tidy(glue::glue("{project_path}/R")))

  flname_dpjournal <- flname_xos_get(fl = "dp_journal.RMD")
  fs::file_copy(path = system.file(flname_dpjournal, package = "dpbuild"),
                new_path = project_path)
  if (!use_targets) {
    fs::file_copy(path = system.file("dp_make.R", package = "dpbuild"),
      new_path = project_path)

    fs::file_copy(path = system.file("global_drake.R", package = "dpbuild"),
      new_path = project_path)
    #TODO: use file_delete if this does not work.
    fs::file_move(path = glue::glue("{project_path}/global_drake.R"),
      new_path = glue::glue("{project_path}/global.R"))

  } else {
    fs::file_copy(path = system.file("_targets.R", package = "dpbuild"),
      new_path = project_path)
    fs::file_move(path = glue::glue("{project_path}/_targets.R"),
      new_path = glue::glue("{project_path}/dp_make.R"))

    fs::file_copy(path = system.file("global_targets.R", package = "dpbuild"),
      new_path = project_path)
    #TODO: use file_delete if this does not work.
    fs::file_move(path = glue::glue("{project_path}/global_targets.R"),
      new_path = glue::glue("{project_path}/global.R"))
  }

  renv::init(bare = T)
  #TODO: Look into which functions to use when adding a package
  #renv::install()

  # commit
  repo <- git2r::repository(path = project_path)
  git2r::add(repo = repo,path =
               fs::path_tidy(glue::glue("{project_path}/dp_make.R")))
  git2r::add(repo = repo,path =
               fs::path_tidy(glue::glue("{project_path}/{flname_dpjournal}")))
  repo <- git2r::commit(repo = repo,
                        message = "Added template code to dp project")
  return(repo)
}

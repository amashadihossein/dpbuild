#' @title Adds script templates
#' @description Adds dp_make.R and dp_journal.RMD to the dp project repo
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

  if (!use_targets) {
    fs::file_copy(path = system.file("dp_make.R", package = "dpbuild"),
      new_path = project_path)

    fs::file_copy(path = system.file("global_drake.R", package = "dpbuild"),
      new_path = glue::glue("{project_path}/R"))
    fs::file_delete(path =  glue::glue("{project_path}/R/global.R"))
    fs::file_move(path = glue::glue("{project_path}/R/global_drake.R"),
      new_path = glue::glue("{project_path}/R/global.R"))
    renv::install(packages = "drake",prompt = F)

    # drake specific journal
    flname_dpjournal <- flname_xos_get(fl = "dp_journal.RMD")
    fs::file_copy(path = system.file(flname_dpjournal, package = "dpbuild"),
                  new_path = project_path)

  } else {
    fs::file_copy(path = system.file("_targets.R", package = "dpbuild"),
      new_path = project_path)
    fs::file_move(path = glue::glue("{project_path}/_targets.R"),
      new_path = glue::glue("{project_path}/dp_make.R"))

    # targets specific journal (renamed)
    flname_dpjournal <- flname_xos_get(fl = "dp_journal_targets.RMD")
    fs::file_copy(path = system.file(flname_dpjournal, package = "dpbuild"),
                  new_path = project_path)
    fs::file_move(path = glue::glue("{project_path}/dp_journal_targets.RMD"),
                  new_path = glue::glue("{project_path}/dp_journal.RMD"))

    fs::file_copy(path = system.file("global_targets.R", package = "dpbuild"),
      new_path = glue::glue("{project_path}/R"))
    fs::file_delete(path =  glue::glue("{project_path}/R/global.R"))
    fs::file_move(path = glue::glue("{project_path}/R/global_targets.R"),
      new_path = glue::glue("{project_path}/R/global.R"))
    renv::install(packages = c("drake", "tarchetypes"), prompt = F)
  }

  # pkgs_dependencies <- renv::dependencies(path = project_path,
  #                                         root = project_path) %>%
  #   dplyr::pull(Package)

  # renv::install(packages = pkgs_dependencies,prompt = F)

  # snapshot with pkgs in global
  renv::snapshot(prompt = F) #TODO: look into  explicitly adding pkgs

  # commit
  repo <- git2r::repository(path = project_path)
  git2r::add(repo = repo, path =
               fs::path_tidy(glue::glue("{project_path}/dp_make.R")))
  git2r::add(repo = repo, path =
               fs::path_tidy(glue::glue("{project_path}/{flname_dpjournal}")))
  git2r::add(repo = repo, path =
               fs::path_tidy(glue::glue("{project_path}/R/global.R")))
  git2r::add(repo = repo, path =
               fs::path_tidy(glue::glue("{project_path}/renv.lock")))
  repo <- git2r::commit(repo = repo,
                        message = "Added template code to dp project")
  return(repo)
}

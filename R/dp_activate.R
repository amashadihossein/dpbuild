#' @title Activate data product project
#' @description Activates renv sandbox and sets the path to project path
#' @param project_path path to the project folder
#' @export
dp_activate <- function(project_path = .){
  if(!is_valid_dp_repository(path  = project_path))
    stop(cli::format_error("Not a valid dp repository! Use dp_init"))
  setwd(project_path)
  renv::activate()
  renv::restore()
}
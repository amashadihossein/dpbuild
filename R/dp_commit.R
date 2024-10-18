#' @title Commit data product
#' @description Runs a number of checks and commits data product changes
#' @param project_path Path to the project folder (default is working directory)
#' @param commit_description Commit message
#' @return repo object post commit
#' @examples \dontrun{
#' dp_commit(commit_description = "First dp commit")
#' }
#' @export
dp_commit <- function(project_path = fs::path_wd(),
                      commit_description) {
  if (length(commit_description) == 0) {
    stop("commit_description needs to have length > 0")
  }

  project_path <- fs::path_tidy(project_path)
  log_path <- fs::path_tidy(glue::glue("{project_path}/.daap/daap_log.yaml"))
  object_path <- get_data_object_path(project_path)

  if (!fs::dir_exists(project_path)) {
    stop("project_path does not exist")
  }

  if (!is_valid_dp_repository(path = project_path)) {
    stop(glue::glue("Not a dp repsitory. Run `dp_repository_check(\"{project_path}\")` for details"))
  }


  if (!file.exists(log_path)) {
    stop("daap_log.yaml was not found. First, complete dp_write")
  }
  log_history <- yaml::read_yaml(file = log_path)
  
  # keeping the references to rds for backword compatibility
  rds_file_sha1 <- digest::digest(object = object_path, algo = "sha1", file = T)


  log_history$HEAD <- as.character(glue::glue("rds_log_{substring(rds_file_sha1, first = 1, last = 7)}"))
  log_history[[log_history$HEAD]]$commit_description <- commit_description
  log_history <- log_history[c("HEAD", setdiff(names(log_history), "HEAD"))]

  yaml::write_yaml(x = log_history, file = log_path)


  # commit
  repo <- git2r::repository(path = project_path)
  add_these <- unlist(git2r::status(repo = repo))

  if (length(add_these) > 0) {
    git2r::add(repo = repo, path = glue::glue("{project_path}/{add_these}"))
    repo <- git2r::commit(repo = repo, all = TRUE, message = commit_description)
  } else {
    print("Nothing new to commit")
  }

  return(repo)
}

get_data_object_path <- function(project_path) {
  # Define possible formats and their paths
  possible_formats <- list(
    RDS = fs::path_tidy(glue::glue("{project_path}/output_files/RDS_format/data_object.RDS")),
    QS = fs::path_tidy(glue::glue("{project_path}/output_files/qs_format/data_object.qs")), 
    ARROW = fs::path_tidy(glue::glue("{project_path}/output_files/arrow_format/data_object.arrow")),
    PARQUET = fs::path_tidy(glue::glue("{project_path}/output_files/parquet_format/data_object.parquet"))
  )
  
  # Iterate through the list and check if the file exists
  for (format in names(possible_formats)) {
    path <- possible_formats[[format]]
    if (file.exists(path)) {
      return(path)
    }
  }
  
  # If no file is found, raise an error
  stop("No data_object file found in any of the expected formats.")
}
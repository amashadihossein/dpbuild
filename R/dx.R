#' @title Determine if valid dp repository
#' @description Looks at the path, runs `dp_repository_check` and returns TRUE
#' if all TRUE
#' @details All diagnostic tests to check validity of dp repository are run
#' regardless of choice of checks. Checks determines what subset is considered
#' in return T/F. Regardless of the choice of checks, verbose = T
#' @param path Path to be evaluated
#' @param checks any combination of c("all","git","dp","renv","branch"). default
#' is all.
#' @param verbose If TRUE, it will print which tests passed/failed
#' @return TRUE or FALSE
#' @export
is_valid_dp_repository <- function(path,
  checks = c("all","git","dp","renv","branch"),
  verbose = F){
  checks <- match.arg(arg = checks,
    choices = c("all","git","dp","renv","branch"),
    several.ok = T)

  dx <- dp_repository_check(path = path)

  if(verbose)
    print(data.frame(dx))


  if(!"all" %in% checks){
    if(!"git" %in% checks)
      dx <- dx[setdiff(names(dx),"git_initialized")]

    if(!"dp" %in% checks)
      dx <- dx[setdiff(names(dx),"dp_initilized")]

    if(!"renv" %in% checks)
      dx <- dx[setdiff(names(dx),"renv_initialized")]

    if(!"branch" %in% checks)
      dx <- dx[setdiff(names(dx),"branch_name_matches")]
  }

  dp_repository <- all(sapply(dx,isTRUE))
  return(dp_repository)
}

#' @title Determine if dp repository initiated
#' @description Looks at the path, runs `dp_repository_check` and returns TRUE
#' if all TRUE
#' @details Any diagnostic tests to check validity of dp repository are run
#' regardless of choice of checks. Checks determines what subset is considered
#' in return T/F. Regardless of the choice of checks, verbose = F
#' @param path Path to be evaluated
#' @param checks any combination of c("git","dp","renv"). default
#' is any.
#' @param verbose If TRUE, it will print which tests passed/failed
#' @return TRUE or FALSE
#' @keywords internal
is_dp_initiated <- function(path,
  checks = c("git","dp","renv"),
  verbose = F){

  checks <- match.arg(arg = checks,
    choices = c("git","dp","renv"),
    several.ok = T)

  dx <- dpbuild:::dp_repository_check(path = path)
  dx[['branch_name_matches']] <- NULL

  if(verbose)
    print(data.frame(dx))

  if(!"git" %in% checks)
    dx <- dx[setdiff(names(dx),"git_initialized")]

  if(!"dp" %in% checks)
    dx <- dx[setdiff(names(dx),"dp_initialized")]

  if(!"renv" %in% checks)
    dx <- dx[setdiff(names(dx),"renv_initialized")]

  check_dx <- sapply(dx, isTRUE)
  dp_initiated <- any(check_dx)

  return(dp_initiated)
}

#' @title Check dp repository
#' @description Runs a number of checks and returns a list of T/F per check
#' @param path Path to be evaluated
#' @return list of T/F per check
#' @keywords internal
dp_repository_check <- function(path){
  dx <- list()
  dx$git_initialized <- git2r::in_repository(path = path)
  dx$dp_initialized <- unname(fs::file_exists(glue::glue("{path}/.daap/daap_config.yaml")))
  dx$renv_initialized <- unname(fs::dir_exists(glue::glue("{path}/renv")))
  dx$branch_name_matches <- NA
  dp_repository <- unname(dx$git_initialized & dx$dp_initialized & dx$renv_initialized)
  if(!dp_repository)
    return(dx)

  repo <- git2r::repository(path = path)
  branch_name <- git2r::repository_head(repo = repo)$name
  dp_conf <- dpconf_read(project_path = path)
  dx$branch_name_matches <- dp_conf$branch_name == branch_name

  return(dx)
}

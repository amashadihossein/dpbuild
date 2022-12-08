#' @title Determine if valid dp repository
#' @description Looks at the path, runs `dp_repository_check` and returns TRUE
#' if all TRUE
#' @details All diagnostic tests to check validity of dp repository are run
#' regardless of choice of checks. Checks determines what subset is considered
#' in return T/F. Regardless of the choice of checks, verbose = T
#' @param path Path to be evaluated
#' @param checks any combination of c("all","git","dp","renv","branch"). default
#' is all.
#' @param check_all_checks checks if all or any values in c("all","git","dp","renv","branch")
#' are to be checked
#' @param verbose If TRUE, it will print which tests passed/failed
#' @return TRUE or FALSE
#' @export
is_valid_dp_repository <- function(path,
                                   checks = c("all","git","dp","renv","branch"),
                                   verbose = F,
                                   check_all_checks = T){
  checks <- match.arg(arg = checks,
                      choices = c("all","git","dp","renv","branch"),
                      several.ok = T)

  dx <- dp_repository_check(path = path)

  if(verbose)
    print(data.frame(dx))

  if(!"all" %in% checks){
    if(!"git" %in% checks)
      dx <- dx[,setdiff(colnames(dx),"git_initialized"), drop = F]

    if(!"dp" %in% checks)
      dx <- dx[,setdiff(colnames(dx),"dp_initilized"), drop = F]

    if(!"renv" %in% checks)
      dx <- dx[,setdiff(colnames(dx),"renv_initialized"), drop = F]

    if(!"branch" %in% checks)
      dx <- dx[,setdiff(colnames(dx),"branch_name_matches"), drop = F]
  }

  if (check_all_checks){
    dp_repository <- all(sapply(dx, isTRUE))
  } else {
    dp_repository <- any(sapply(dx, isTRUE))
  }
  return(dp_repository)
}


#' @title Check dp repository
#' @description Runs a number of checks and returns a list of T/F per check
#' @param path Path to be evaluated
#' @return list of T/F per check
#' @keywords internal
dp_repository_check <- function(path){
  dx <- list()
  dx$git_initialized <- git2r::in_repository(path = path)
  dx$dp_initilized <- fs::file_exists(glue::glue("{path}/.daap/daap_config.yaml"))
  dx$renv_initialized <- fs::dir_exists(glue::glue("{path}/renv"))
  dx$branch_name_matches <- NA
  dp_repository <- unname(dx$git_initialized & dx$dp_initilized & dx$renv_initialized)
  if(!dp_repository)
    return(dx)

  repo <- git2r::repository(path = path)
  branch_name <- git2r::repository_head(repo = repo)$name
  dp_conf <- dpconf_read(project_path = path)
  dx$branch_name_matches <- dp_conf$branch_name == branch_name

  return(dx)
}

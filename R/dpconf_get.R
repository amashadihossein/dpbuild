#' @title Get daap config
#' @description Reads in and returns daap config file hydrating any expression 
#' in the process.
#' @details This function reads in the yaml config as a list. in the process, it
#' hydrate any expression for `board_params` and `creds`. Make sure, environment 
#' variables declared in dried functions are set prior to calling `dpconf_get`.
#' @param project_path path to project folder
#' @return a list dpconf
#' @export

dpconf_get <- function(project_path){
  
  dpconf <- dpconf_read(project_path = project_path)
  
  dpconf$board_params <- 
    fn_hydrate(glue::glue_collapse(dpconf$board_params_set_dried))
  dpconf$creds <- fn_hydrate(glue::glue_collapse(dpconf$creds_set_dried))
  
  dpconf$board_params_set_dried <- dpconf$creds_set_dried <- NULL
  
  # if(!is.null(dpconf$githubpat_set_dried)){
  #   dpconf$githubpat <- fn_hydrate(glue::glue_collapse(dpconf$githubpat_set_dried))
  #   dpconf$githubpat_set_dried <- NULL
  # }
    
  dpconf <- list2env(x = dpconf, envir = new.env())
  
  class(dpconf) <- c(dpconf$board_params$board_type, class(dpconf))
  
  #TODO validate dpconf
  dpconf_validate(dpconf = dpconf, project_path = project_path)
  
  return(dpconf)
}


#' @title Read daap config
#' @description Read daap_config.yaml
#' @param project_path path to project folder
#' @return a list of raw dpconf where any function call remains dry
#' @keywords internal
dpconf_read <- function(project_path){
  dpconf <- 
    yaml::read_yaml(file = glue::glue("{project_path}/.daap/daap_config.yaml"))
  return(dpconf)
}



#' @title Write daap config
#' @description Write in and returns daap config file
#' @param project_path path to project folder
#' @return a list dpconf
#' @keywords internal
dpconf_write <- function(project_path, dpconf){
  yaml::write_yaml(x = dpconf , 
                   file = glue::glue("{project_path}/.daap/daap_config.yaml"))
  return(dpconf)
}


#' @title validate dpconf
#' @description This function errors if invalid conf
#' @param dpconf a dp config
#' @param project_path path to project
#' @keywords internal
dpconf_validate <- function(dpconf, project_path){
  #TODO: add additional validation for repo test as well as data remote test
  # specific to class of config
  
  if(nchar(Sys.getenv("GITHUB_PAT")) == 0)
    stop(cli::format_error(glue::glue("Could not get dpconf as it did not find", 
                                      " GITHUB_PAT in the environment. Set it ",
                                      "by Sys.setenv(GITHUB_PAT = <your github",
                                      " personal access token>) and retry!")))
  
  repo <- git2r::repository(path = project_path)
  repo_url <- git2r::remote_url(repo = repo)
  
  if(!is.null(repo_url)){
    repo_resp <-
      try(gh::gh_whoami(.api_url = repo_url, .token = Sys.getenv("GITHUB_PAT")))
    
    if("try-error" %in% class(repo_resp))
      warning(cli::format_warning(glue::glue("For the given repo and ",
                                             "GITHUB_PAT, failed to connect",
                                             " to the GITHUB API. Check env ",
                                             "variable GITHUB_PAT and remote ",
                                             "repo url!")))
  }
  
  
}
#' @title write data product input manifest
#' @description Converts structured input into a yaml manifest
#' @param input_d a list with synced_input elements structured as desired within
#'  a list
#' @param project_path Project path
#' @param verbose T/F
#' @return TRUE
#' @export

dpinput_write <- function(project_path, input_d, verbose = F){

  if(verbose)
    print("Writing daap input yaml")

  if(!is_valid_dp_repository(path = project_path))
    stop(cli::format_error(glue::glue("Not a dp repsitory. Run ",
                                      "`dp_repository_check(\"{project_path}\")`",
                                      " for details")))

  dpinput_make(input_d = input_d) %>%
    yaml::write_yaml(x = .,
                     file =  glue::glue("{project_path}/.daap/daap_input.yaml"))

  return(TRUE)
}


#' @title make data product input manifest
#' @description Converts structured input into a yaml manifest
#' @param input_d a list with synced_input elements structured as desired within
#'  a list
#' @return TRUE
#' @keywords internal
dpinput_make <- function(input_d){

  # TODO: remove reliance on name metadata for identifying leaf. 
  # This may be fragile as metadata may not necessarily be the one we think
  daap_input <- purrr::map(input_d, .f = function(x){
    if(is.list(x)){
      if(!"metadata" %in% names(x)){
        return(dpinput_make(input_d = x))
      }else{
        return(as.list(x$metadata))
      }
    }
  })

  return(daap_input)
}

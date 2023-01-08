#' @title Get last dpinput
#' @description Build the last dpinput from daap_inpt.yaml recorded
#' @param project_path current project path
#' @return input_map0 a data.frame that maps the content of the already synced
#' input content or NULL if no daap_input.yaml
#' @keywords internal

dpinput_map0 <- function(project_path = ".") {
  if (!fs::file_exists(glue::glue("{project_path}/.daap/daap_input.yaml"))) {
    return(NULL)
  }

  daap_input_yaml <-
    yaml::read_yaml(file = glue::glue("{project_path}/.daap/daap_input.yaml"))
  read_dpinput0 <-
    dpinput_read(daap_input_yaml = daap_input_yaml, add_metadata = T)
  input_obj0 <- dpinput_flatten(read_dpinput0)
  input_manifest0 <- input_obj0 %>%
    purrr::map(~ purrr::pluck(.x, "metadata")) %>%
    dplyr::bind_rows()

  input_map0 <- list(input_obj = input_obj0, input_manifest = input_manifest0)

  return(input_map0)
}


#' @title flatten the read dpinput in the format of dp_map
#' @description recursively traverses list out of `dir_process` and flattens the
#'  list of read data
#'  capturing the structure as path name
#' @param read_dpinput0 out of call to `dir_process`
#' @return a one level deep list of data tables with name of each element
#' capture the folder structure info
#' @keywords internal
dpinput_flatten <- function(read_dpinput0) {
  simple_list <- NULL

  if (length(index_simple <-
    which(sapply(read_dpinput0, class) %in% c("dpinput"))) > 0) {
    simple_list <- read_dpinput0[index_simple]
    names(simple_list) <- purrr::map(.x = names(simple_list), .f = function(x) {
      simple_list[[x]][["metadata"]][["id"]]
    }) %>% unlist()
  }


  if (length(index_deep <-
    which(!sapply(read_dpinput0, class) %in% c("dpinput"))) > 0) {
    deep_list <- read_dpinput0[index_deep]
    tmp <- dpinput_flatten(read_dpinput0 = unlist(deep_list,
      use.names = T, recursive = F
    ))
    simple_list <- c(simple_list, tmp)
  }

  return(simple_list)
}

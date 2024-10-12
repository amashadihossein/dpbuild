#' @title Write data product
#' @description This function writes the data product and logs it in `.daap/daap_log.yaml`
#' @param data_object data_object generated from `dp_structure`
#' @param project_path path to the project (default is current directory)
#' @param type File type used to save the data product, default RDS
#' @return TRUE
#' @export
dp_write <- function(data_object, type = 'rds', project_path = ".") {
  if (!is_valid_dp_repository(path = project_path)) {
    stop(glue::glue(
      "Not a dp repsitory. ",
      "Run `dp_repository_check(\"{project_path}\")` ",
      "for details"
    ))
  }

  if (!"dp" %in% class(data_object)) {
    stop(cli::format_error(glue::glue(
      "dp_write requires and data_object of ",
      "class dp! Use dp_structure to build ",
      "a properly formatted dp"
    )))
  }

  dataobj_path <- save_object(data_object, project_path = project_path, type = type)

  log_note <- dplognote_get(
    data_object = data_object,
    dataobj_path = dataobj_path
  )
  log_label <- names(log_note)[[1]]

  log_history <- log_note

  if (file.exists(glue::glue("{project_path}/.daap/daap_log.yaml"))) {
    log_history <- yaml::read_yaml(file = glue::glue(
      "{project_path}/",
      ".daap/daap_log.yaml"
    ))

    if (log_label %in% names(log_history)) {
      if (!identical(log_history[log_label], log_note)) {
        log_history[log_label] <- log_note
      }
    } else {
      log_history <- c(log_history, log_note)
    }
  }

  yaml::write_yaml(x = log_history, file = glue::glue(
    "{project_path}/",
    ".daap/daap_log.yaml"
  ))

  return(TRUE)
}

save_object <- function(data_object, project_path, type = "rds"){
  type <- arg_match0(type, setdiff(object_types, "file"))

switch(type,
    rds = write_rds(x, project_path),
    qs = write_qs(x, project_path)
  )

}

write_qs <- function(data_object, project_path) {
  check_installed("qs")
  dataobj_path <- glue::glue(
    "{project_path}/",
    "output_files/qs_format/data_object.qs"
  )
  check_dir(dataobj_path)
  qs::qsave(data_object, dataobj_path)
  return(dataobj_path)
}

write_rds <- function(data_object, project_path) {
  dataobj_path <- glue::glue(
    "{project_path}/",
    "output_files/RDS_format/data_object.RDS"
  )
  check_dir(dataobj_path)
  saveRDS(object = data_object, file = dataobj_path, version = 2)
  return(dataobj_path)
}

object_types <- c("rds", "qs")

check_dir <- function(path){
  if (!dir.exists(paths = path)) {
    dir.create(
      path = path,
      recursive = T
    )
  }
}


#' @title Get log note from data object
#' @description This builds log note
#' @param data_object data_object
#' @param dataobj_path path to the data object
#' @return log_note
#' @keywords internal
dplognote_get <- function(data_object, dataobj_path) {
  
  attrs <- purrr::list_modify(attributes(data_object), names = purrr::zap())
  rds_file_sha1 <- digest::digest(object = dataobj_path, algo = "sha1", file = T)

  data_object_pin_version <- get_pin_version(
    d = data_object,
    pin_name = attr(data_object, "dp_name"),
    pin_description = attr(
      data_object,
      "branch_description"
    )
  )

  # pin_version_split <- unlist(base::strsplit(x = data_object_pin_version, split = "-"))
  # pin_hash <- pin_version_split[length(pin_version_split)]

  log_note <- c(attrs,
    rds_file_sha1 = rds_file_sha1,
    pin_version = data_object_pin_version
  )
  sha1_short <- substring(log_note$rds_file_sha1, first = 1, last = 7)
  log_label <- glue::glue("rds_log_{sha1_short}")
  log_note <- list(log_note)
  names(log_note)[[1]] <- log_label

  return(log_note)
}

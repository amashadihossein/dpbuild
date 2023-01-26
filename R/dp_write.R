#' @title Write data product
#' @description This function writes the data product and logs it
#' @param data_object data_object
#' @param project_path path to the project
#' @return TRUE
#' @export
dp_write <- function(data_object, project_path = ".") {
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

  # Store the output
  if (!dir.exists(paths = glue::glue("{project_path}/output_files/RDS_format"))) {
    dir.create(
      path = glue::glue("{project_path}/output_files/RDS_format"),
      recursive = T
    )
  }

  dataobj_path <- glue::glue(
    "{project_path}/",
    "output_files/RDS_format/data_object.RDS"
  )
  saveRDS(object = data_object, file = dataobj_path)
  data_object <- readRDS(file = dataobj_path)

  log_note <- dplognote_get(
    data_object = data_object,
    project_path = project_path
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


#' @title Get log note from data object
#' @description This builds log note
#' @param data_object data_object
#' @param project_path path to the project
#' @return log_note
#' @keywords internal
dplognote_get <- function(data_object, project_path) {
  dataobj_path <- glue::glue(
    "{project_path}/",
    "output_files/RDS_format/data_object.RDS"
  )
  attrs <- purrr::list_modify(attributes(data_object), names = purrr::zap())
  rds_file_sha1 <- digest::digest(object = dataobj_path, algo = "sha1", file = T)
  rds_obj_sha1 <- digest::sha1(
    x = make_sha1_compatible(data_object),
    environment = F
  )
  pin_version <- get_pin_version(
    d = data_object,
    pin_name = attr(data_object, "dp_name"),
    pin_description = attr(
      data_object,
      "branch_description"
    )
  )

  log_note <- c(attrs,
    rds_file_sha1 = rds_file_sha1,
    rds_obj_sha1 = rds_obj_sha1, pin_version = pin_version
  )
  sha1_short <- substring(log_note$rds_file_sha1, first = 1, last = 7)
  log_label <- glue::glue("rds_log_{sha1_short}")
  log_note <- list(log_note)
  names(log_note)[[1]] <- log_label

  return(log_note)
}

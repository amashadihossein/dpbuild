#' @title Map data input into data product workflow
#' @description This is helper function to help manage the context of
#' input_files folder
#' @param project_path path to the project
#' @return input_map a data.frame that maps the content of the input_files to be
#'  evaluated
#' @export
dpinput_map <- function(project_path) {
  input_dir <- glue::glue("{project_path}/input_files/")
  # TODO: extend the same workflow seamlessly to a single data file
  if (!fs::dir_exists(input_dir)) {
    stop(cli::format_error(glue::glue(
      "input_dir {input_dir} specified does ",
      "not exist. Check the path"
    )))
  }

  check_pins_compatibility()

  if (length(fs::dir_ls(input_dir)) == 0) {
    message("input_files directory is empty. It is OK to have an empty input_files directory if there is no need to update input data.")
  }

  read_files <- dir_process(current_dir = input_dir)
  flattened_dirTree <- dirTree_flatten(read_files = read_files)
  input_obj <- dirTree_build(flattened_dirTree = flattened_dirTree)

  # Get the last dpinput_map from dp log
  input_map0 <- dpinput_map0(project_path = project_path)

  if (length(input_map0) > 0) {
    input_manifest0 <- input_map0$input_manifest %>%
      dplyr::mutate(id0 = glue::glue("{.data$path}/{substr(file_sha1,start=1,stop = 7)}")) %>%
      dplyr::mutate(restaged_items = .data$id0 %in% names(input_obj))

    if (nrow(restaged <- input_manifest0 %>% dplyr::filter(.data$restaged_items)) > 0) {
      input_obj[restaged$id0] <- NULL
      input_obj <- c(input_obj, input_map0$input_obj[restaged$id])
    }


    if (nrow(newlystaged <- input_manifest0 %>% dplyr::filter(!.data$restaged_items)) > 0) {
      input_obj <- c(input_obj, input_map0$input_obj[newlystaged$id])
    }
  }


  input_manifest <- input_obj %>%
    purrr::map(~ purrr::pluck(.x, "metadata")) %>%
    dplyr::bind_rows()

  input_manifest_failed <- input_manifest %>%
    dplyr::filter(.data$file_read_fail) %>%
    dplyr::mutate(to_be_synced = F) %>%
    dplyr::distinct()


  input_manifest_read <- input_manifest %>%
    dplyr::filter(!.data$file_read_fail)

  input_manifest_read <- input_manifest_read %>%
    dplyr::group_by(.data$data_sha1) %>%
    dplyr::count() %>%
    dplyr::left_join(input_manifest_read, .) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$data_sha1, dplyr::desc(.data$synced)) %>%
    dplyr::mutate(to_be_synced = !duplicated(.data$data_sha1)) %>%
    dplyr::rename(n_dupe_datasha1 = .data$n) %>%
    dplyr::group_by(.data$data_sha1) %>%
    dplyr::mutate(n_name_per_datasha1 = dplyr::n_distinct(.data$name)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$name) %>%
    dplyr::mutate(n_datasha1_per_name = dplyr::n_distinct(.data$data_sha1)) %>%
    dplyr::select(
      .data$id, .data$path, .data$name, .data$file_name, .data$file_sha1,
      .data$data_sha1, .data$file_read_fail, .data$n_dupe_datasha1,
      .data$n_name_per_datasha1, .data$n_datasha1_per_name, .data$description,
      .data$pin_version, .data$synced, .data$to_be_synced
    ) %>%
    dplyr::mutate(synced = tidyr::replace_na(.data$synced, F)) %>%
    dplyr::ungroup()


  input_manifest <- dplyr::bind_rows(input_manifest_read, input_manifest_failed)

  input_map <- list(input_obj = input_obj, input_manifest = input_manifest)

  return(input_map)
}

#' @title Get tidy directory list
#' @description Given a directory returns directory list as a custom tidy tibble
#' @param current_dir path to the current directory
#' @return ls_tidy a tibble
#' @keywords internal
dir_ls_tidy <- function(current_dir) {
  ls_tidy <- tibble::tibble(path = fs::dir_ls(current_dir)) %>%
    dplyr::mutate(ext = fs::path_ext(.data$path)) %>%
    dplyr::mutate(is_file = fs::is_file(.data$path)) %>%
    dplyr::mutate(is_dir = fs::is_dir(.data$path)) %>%
    dplyr::mutate(content_type = dplyr::case_when(
      is_file & ext != "zip" ~ "file",
      is_file & ext == "zip" ~ "zip",
      is_dir ~ "dir"
    )) %>%
    dplyr::select(.data$path, .data$ext, .data$content_type)

  return(ls_tidy)
}

#' @title Process current input directory
#' @description Recursively reads and hashes the content of the current
#' directory returning the content as a list that matches the structure of the
#' directory
#' @param current_dir path to the current directory
#' @param junk_path path to be dropped from the path prefix
#' it prevents repeating the capture of folder structure already captured
#' @return read_files a list of read contents
#' @keywords internal
dir_process <- function(current_dir, junk_path = character(0)) {
  # TODO: build additional flexibility beyond rio::import for reading files.
  # Can custom function be passed?

  read_files <- NULL

  ls_tidy <- dir_ls_tidy(current_dir = current_dir)

  if ("file" %in% ls_tidy$content_type) {
    tmp <- ls_tidy %>%
      dplyr::filter(.data$content_type == "file") %>%
      dplyr::select(.data$path) %>%
      purrr::pmap(.l = ., .f = function(path) {
        # TODO: change the tibble data.frame. need to deal with better plucking
        # so it can handle inherited classes
        tb_i <- try(as.data.frame(tibble::tibble(rio::import(file = path))))

        if ("try-error" %in% class(tb_i)) {
          tb_i <- data.frame()
          attr(tb_i, "data_sha1") <- NA
          attr(tb_i, "file_sha1") <- NA
          attr(tb_i, "file_read_fail") <- T
        } else {
          attr(tb_i, "data_sha1") <- tbsig_get(d = tb_i)
          attr(tb_i, "file_sha1") <- digest::digest(file = path, algo = "sha1")
          attr(tb_i, "file_read_fail") <- F
        }
        tb_i
      })

    if (length(junk_path) > 0) {
      names(tmp) <- sub(pattern = junk_path, replacement = "", x = names(tmp))
    }

    read_files <- c(read_files, tmp)
  }


  if ("zip" %in% ls_tidy$content_type) {
    tmp <- ls_tidy %>%
      dplyr::filter(.data$content_type == "zip") %>%
      dplyr::select(.data$path) %>%
      purrr::pmap(.l = ., .f = function(path) dir_process_zip(zip_dir = path))

    if (length(junk_path) > 0) {
      names(tmp) <- sub(pattern = junk_path, replacement = "", x = names(tmp))
    }

    read_files <- c(read_files, tmp)
  }


  if ("dir" %in% ls_tidy$content_type) {
    tmp <- ls_tidy %>%
      dplyr::filter(.data$content_type == "dir") %>%
      dplyr::select(.data$path) %>%
      purrr::pmap(.l = ., .f = function(path) {
        dir_process(
          current_dir = path,
          junk_path = path
        )
      })

    if (length(junk_path) > 0) {
      names(tmp) <- sub(pattern = junk_path, replacement = "", x = names(tmp))
    }

    read_files <- c(read_files, tmp)
  }

  return(read_files)
}


#' @title Process a zip directory
#' @description This function simply extends the functionality of `dir_process`
#' to zipped directories.
#' it unzips in temp directory and once unzipped calls `dir_process`
#' @param zip_dir path to the zipped directory
#' @return read_files a list of read contents
#' @keywords internal
dir_process_zip <- function(zip_dir) {
  exdir <- tempfile()
  dir_created <- fs::dir_create(path = exdir)
  unzipped <- utils::unzip(zipfile = zip_dir, exdir = dir_created)
  current_dir <- fs::path_dir(path = unzipped) %>% unique()
  highest_common_dir <- fs::path_common(current_dir)
  read_files <- dir_process(
    current_dir = highest_common_dir,
    junk_path = dir_created
  )

  return(read_files)
}


#' @title flatten a directory tree
#' @description recursively traverses list out of `dir_process` and flattens the
#'  list of read data
#'  capturing the structure as path name
#' @param read_files out of call to `dir_process`
#' @return a one level deep list of data tables with name of each element
#' capture the folder structure info
#' @keywords internal
dirTree_flatten <- function(read_files) {
  simple_list <- NULL

  if (length(index_simple <- which(sapply(read_files, class) == "data.frame")) > 0) {
    simple_list <- read_files[index_simple]
  }

  if (length(index_deep <- which(sapply(read_files, class) != "data.frame")) > 0) {
    deep_list <- read_files[index_deep]
    tmp <- dirTree_flatten(read_files = unlist(deep_list,
      use.names = T,
      recursive = F
    ))
    simple_list <- c(simple_list, tmp)
  }

  return(simple_list)
}


#' @title Build directory tree
#' @description Receives a flattened directory tree from `dirTree_flatten` and
#' appends a metadata tibble to each element
#' @param flattened_dirTree  out of call to `dirTree_flatten`
#' @return dirTree a similar to output of `dirTree_flatten` but with each
#' element enriched with tidy metadata including
#' sha1 and original path to each data
#' @keywords internal
dirTree_build <- function(flattened_dirTree) {
  dirTree <- names(flattened_dirTree) %>%
    purrr::map(.x = ., .f = function(pathname) {
      tmp <- list(data = flattened_dirTree[[pathname]])
      tmp$metadata <-
        attributes(tmp$data)[c("class", "data_sha1", "file_sha1", "file_read_fail")] %>%
        dplyr::bind_cols() %>%
        dplyr::mutate(file_name = basename(pathname)) %>%
        dplyr::mutate(name = tools::file_path_sans_ext(.data$file_name)) %>%
        dplyr::mutate(name = make_names_codefriendly(.data$name)) %>%
        dplyr::mutate(path = pathname) %>%
        dplyr::mutate(id = glue::glue("{path}/{substr(file_sha1,start=1,stop = 7)}")) %>%
        dplyr::mutate(id = as.character(.data$id)) %>%
        dplyr::mutate(description = NA, pin_version = NA, synced = NA) %>%
        dplyr::select(
          .data$id, .data$name, .data$path, .data$file_name,
          .data$file_sha1, .data$data_sha1, .data$file_read_fail,
          .data$class, .data$description, .data$pin_version, .data$synced
        )

      attr(tmp$data, "data_sha1") <- NULL
      attr(tmp$data, "file_sha1") <- NULL
      attr(tmp$data, "file_read_fail") <- NULL
      tmp
    })

  names(dirTree) <- purrr::map(dirTree, ~ .x$metadata$id) %>% unlist()

  return(dirTree)
}


#' @title Reset sync flag in the manifest
#' @description It updates input_map manifest and reverses the to_be_synced flag
#'  state
#' @param input_map input_map made with `dpinput_map`
#' @param input_id a vector of character strings matching
#' `input_mapt$input_manifest$id`
#' @return modified `input_map`
#' @export
dpinput_syncflag_reset <- function(input_map, input_id) {
  if (any(duplicated(input_map$input_manifest$id))) {
    warning("input_map$input_manifest$id has duplicates!")
  }

  if (any(!input_id %in% input_map$input_manifest$id)) {
    missing_input_ids <-
      paste0(setdiff(input_id, input_map$input_manifest$id), collapse = ", ")

    stop(cli::format_error(glue::glue(
      "input_id elements {missing_input_ids} ",
      "are not in input_mapt$input_manifest$id"
    )))
  }


  id_index <- which(input_map$input_manifest$id %in% input_id)

  input_map$input_manifest$to_be_synced[id_index] <-
    !input_map$input_manifest$to_be_synced[id_index]
  return(input_map)
}

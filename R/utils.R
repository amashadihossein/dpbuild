#' @title Get Data Product Name
#' @description A helper function that build data product name (i.e. what the pin is going to be called)
#' @param data_object Data Object. It anticipates project_name and branch_name to be attributes of this data object
#' @return dp_name a character that will be tagged back as attribute to data_object
#' @export
dpname_get <- function(data_object) {
  dp_name <- dpname_make(
    project_name = attributes(data_object)$project_name,
    branch_name = attributes(data_object)$branch_nam
  )
  return(dp_name)
}


#' @title Make Data Product Name
#' @description A helper function that build data product name (i.e. what the pin is going to be called)
#' @param project_name Project name
#' @param branch_name Branch name
#' @return dp_name a character that will be tagged back as attribute to data_object
#' @export
dpname_make <- function(project_name, branch_name) {
  dp_name <- glue::glue("{project_name}_{branch_name}")
  dp_name <- gsub(pattern = "_", replacement = "-", dp_name)
  dp_name <- as.character(dp_name)
  return(dp_name)
}


#' @title Get Pins Version Pre Deploy
#' @description  This get the pins version pre-deploy
#' @param d data object
#' @param pin_name what the pin will be named. For data products, it is encoded in dp_param
#' @param pin_description what the pin description will be. For data products, it is encoded in dp_params
#' @return a character version
#' @importFrom dplyr .data
#' @keywords internal
get_pin_version <- function(d, pin_name, pin_description) {
  withr::local_options(list(pins.quiet = TRUE))
  pin_name <- as.character(pin_name)
  pin_description <- as.character(pin_description)

  # local_board_folder <- pins::board_folder(path = "daap_internal", versioned = T)
  temp_board_folder <- pins::board_temp(versioned = T)

  pin_name_exists <- pins::pin_exists(board = temp_board_folder, name = pin_name)

  if (pin_name_exists) {
    pins::pin_delete(names = pin_name, board = temp_board_folder)
  }

  pins::pin_write(
    x = d,
    name = pin_name,
    board = temp_board_folder,
    description = pin_description
  )

  pin_version <- pins::pin_versions(
    name = pin_name,
    board = temp_board_folder
  ) %>% dplyr::pull(.data$hash)
  pins::pin_delete(names = pin_name, board = temp_board_folder)

  return(pin_version)
}


#' @title Get Readme to be appended to the data object
#' @description  This function builds readme metadata
#' @param d data object
#' @param general_note a character string to be added to general notes for this data object branch/commit
#' @return readme text
#' @export
readme_get <- function(d, general_note) {
  readme <- list()
  readme$general_note <- general_note
  readme$input <- paste("Input data includes", paste(setdiff(names(d$input), "metadata"), collapse = ", "))

  readme$output <- paste("This contains the following:", paste(names(d$output), collapse = ", "))

  readme$exploratory <- paste("This contains the following:", paste(names(d$exploratory), collapse = ", "))

  readme$metadata <- paste("This contains the following:", paste(names(d$metadata), collapse = ", "))

  readme <- readme[c("general_note", setdiff(names(d), "README"))]

  # readme$session_info <- sessionInfo()

  return(readme)
}


# tbsig_get <- function(x){
#   sha1.hms <<- sha1.difftime <<- function (x, digits = 14L, zapsmall = 7L, ..., algo = "sha1") {
#
#     digest:::sha1.character(x = x,
#                             digits = digits, zapsmall = zapsmall, ..., algo = algo)
#   }
#   tbsig <- digest::sha1(x = x, environment = F)
#   return(tbsig)
# }


#' @title Get sha1 signature for a table
#' @description  This function is a wrapper around digest::sha1 to handle exotic column classes
#' @param d data.frame
#' @return tbsig a character string
#' @export
tbsig_get <- function(d) {
  if (!inherits(d, "data.frame")) {
    stop("tbsig is only for data.frames. Ensure d is a data.frame")
  }

  supported_classes <- c(
    "numeric", "integer", "character",
    "factor", "Date", "logical", "POSIXlt",
    "POSIXct"
  )
  d1 <- as.data.frame(lapply(d, FUN = function(col_i) {
    this_class <- class(col_i)
    if (any(!this_class %in% supported_classes)) {
      class(col_i) <- c(this_class, setdiff("character", this_class))
    }
    col_i
  }))

  attributes(d1) <- attributes(d)

  tbsig <- digest::sha1(x = d1, environment = F)

  return(tbsig)
}


#' @title Update exotic column classes of data.frames in a list for sha1 signature
#' @description  This function is to be used before digest::sha1 to handle exotic column classes
#' @param l list
#' @return list
#' @keywords internal
make_sha1_compatible <- function(l) {
  if (setdiff(class(l), "dp") != "list") {
    stop("lssig is only for lists. Ensure l is a list")
  }

  supported_classes <- c(
    "numeric", "integer", "character",
    "factor", "Date", "logical", "POSIXlt",
    "POSIXct"
  )

  l1 <- lapply(l, FUN = function(node) {
    if (any(class(node) == "data.frame")) {
      d1 <- as.data.frame(lapply(node, FUN = function(col_i) {
        this_class <- class(col_i)
        if (any(!this_class %in% supported_classes)) {
          class(col_i) <- c(this_class, setdiff("character", this_class))
        }
        col_i
      }))

      attributes(d1) <- attributes(node)
      d1
    } else if (any(class(node) == "list")) {
      make_sha1_compatible(node)
    } else {
      node
    }
  })

  return(l1)
}


#' @title Make names code friendly
#' @description  This function tries to provide a more sensible mapping of names to their code friendly version than make.names
#' @param x a character string to be converted
#' @param make_unique if TRUE it ensures each element of a vector names end up being unique
#' @return the code friendly converted character string
#' @export
make_names_codefriendly <- function(x, make_unique = T) {
  x <- trimws(x) %>%
    paste0(ifelse(grepl(pattern = "^[0-9]", x = .), "var_", ""), .) %>%
    gsub("(?<![0-9])\\-", "-", x = ., perl = T) %>%
    gsub("\\&", "_and_", x = .) %>%
    gsub("\\@", "_at_", x = .) %>%
    gsub("\\+", "_pos_", x = .) %>%
    gsub(",", "_comma_", x = .) %>%
    gsub("\\/", "_fwdslsh_", x = .) %>%
    gsub("\\(|\\)", "\\.", x = .) %>%
    gsub("\\%", "percent_", x = .) %>%
    gsub("\\#", "num_", x = .) %>%
    gsub(" ", "_", x = .)

  if (make_unique) {
    return(make.names(names = x, unique = T))
  }

  return(make.names(names = x, unique = F))
}


#' @title Make dpinput names simplified
#' @description  This function tries to drop the full descriptive name of dpinput
#' elements for code aesthetics
#' @param x a character string of the form `{path}/{file_name.extension}/{sha1}`
#' which will be converted to a character string of the form `{file_name}`
#' @param make_unique if TRUE it ensures each element of a vector names end up
#' being unique. If not it errors if not simplified names not unique.
#' @return the code friendly converted character string
#' @keywords internal
dpinputnames_simplify <- function(x, make_unique = FALSE) {
  simplified_inputnames <- fs::path_split(x) %>%
    sapply(X = ., function(x) {
      x_trimmed <- x
      if (length(x) > 1) {
        x_trimmed <- fs::path_ext_remove(rev(x)[2])
      }
      x_trimmed
    })

  if (make_unique) {
    return(make.unique(simplified_inputnames))
  }

  if (any(dups <- duplicated(simplified_inputnames))) {
    stop(paste("simplified names", paste0(which(dups), collapse = ", "), "are dupclicate which are not allowed"))
  }

  return(simplified_inputnames)
}


#' @title Clean input_map
#' @description  This function drops unsynced inputs from the input map
#' and cleans names
#' @param input_map synced mapped object as returned by `dpbuild::dpinput_map`
#' @param remove_id a vector of input_data ids to remove. This is for convenience
#' as setting the input_manifest field `to_be_synced` to FALSE can achieve the
#' same thing. The default value of `character(0)` limits removal to any row
#' with `to_be_synced == FALSE`
#' @param force_cleanname T/F, if TRUE it ensures each input id name ends up
#' being unique. If FALSE, it won't clean names unless names are already unique
#' @return input_map pruned and with cleaner names
#' @export
inputmap_clean <- function(input_map, remove_id = character(0), force_cleanname = F) {
  input_map$input_manifest <- input_map$input_manifest %>%
    dplyr::mutate(to_be_synced = replace(.data$to_be_synced, .data$id %in% remove_id, FALSE))

  input_map$input_manifest <- input_map$input_manifest %>% dplyr::filter(.data$to_be_synced)
  input_map$input_obj <- input_map$input_obj[input_map$input_manifest$id]

  if (!inherits(try(dpinputnames_simplify(input_map$input_manifest$id)), "try-error") | force_cleanname) {
    input_map$input_manifest <- input_map$input_manifest %>%
      dplyr::mutate(id = dpinputnames_simplify(.data$id, make_unique = force_cleanname))

    names(input_map$input_obj) <-
      dpinputnames_simplify(names(input_map$input_obj),
        make_unique = force_cleanname
      )

    input_map$input_obj <- sapply(names(input_map$input_obj), function(name_i) {
      input_map$input_obj[[name_i]]$metadata$id <- name_i
      input_map$input_obj[[name_i]]
    }, simplify = F, USE.NAMES = T)
  }

  return(input_map)
}


#' @title Purge Local Pins Cache
#' @description  It completely deletes content of local cache. Use with care!
#' @param path_cache path to pins cache. Default is `pins::board_cache_path()`
#' @keywords internal
purge_local_cache <- function(path_cache = pins::board_cache_path()) {
  fs::dir_delete(fs::dir_ls(path_cache))
}

#' @title Gets cross OS File Name
#' @description  It drops extension that can be OS-specific
#' @param fl just the file name e.g. README.RMD
#' @param package package name e.g. dpbuild
#' @keywords internal
flname_xos_get <- function(fl, package = "dpbuild") {
  pkg_path <- system.file(package = package)
  fl_name <- fs::path_ext_remove(fl)
  fl_path <- Sys.glob(glue::glue("{pkg_path}/{fl_name}.*"))
  flname <- basename(fl_path)
  return(flname)
}


#' @title Check pins package compatibility
#' @description Check pins package compatibility
#' @param is_legacy A boolean variable to indicate if the pins version is legacy one
#' @keywords internal
check_pins_compatibility <- function(){
  read_conf_file <- dpbuild:::dpconf_read(project_path = ".")

  if ("is_legacy" %in% names(read_conf_file)) {
    is_legacy_dp <- read_conf_file$is_legacy
  } else {
    is_legacy_dp <- T
  }

  is_installed_pins_version_legacy <- utils::packageVersion(pkg = "pins") < '1.2.0'

  pins_version_message <- glue::glue(
    'This data product was built with a legacy version of pins.
    Please downgrade pins and all daapr packages using
    remotes::install_github(repo = "amashadihossein/pins")
    remotes::install_github(repo = "amashadihossein/dpi@v0.0.0.9008")
    remotes::install_github(repo = "amashadihossein/dpbuild@v0.0.0.9106")
    remotes::install_github(repo = "amashadihossein/dpdeploy@v0.0.0.9016")
    remotes::install_github(repo = "amashadihossein/daapr@v0.0.0.9006")'
  )

  if (any(is_legacy_dp,is_installed_pins_version_legacy)) {

    stop(cli::cli_alert_danger(pins_version_message))
  }
}


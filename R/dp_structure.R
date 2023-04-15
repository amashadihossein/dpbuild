#' @title Structure a data product
#' @description This function assembles the data product and properly assigns
#' class and attributes. For examples, use `dpbuild::dpcode_add` to generate a dp_make.R file
#' @param data_files_read object generated from `dpbuild::dpinput_read()`
#' containing links to input data
#' @param config data product config file from `dpbuild::dpconf_get`
#' @param output a list of content to be structured under output
#' @param metadata a list of content to be structured under metadata
#' @return a list containing README, raw_input, input, and output
#' @export
dp_structure <- function(data_files_read, config, output = list(), metadata = list()) {
  config <- as.list(config)

  # Initiate the structure of the data
  data_object <- list(
    README = character(0),
    input = data_files_read,
    output = output,
    metadata = metadata
  )

  data_object$README <- dpbuild::readme_get(data_object, general_note = config$readme_general_note)

  data_object <- data_object[sapply(data_object, length) > 0]

  attrs_to_add <- c(
    "project_name", "project_description", "branch_name",
    "branch_description"
  )
  attributes(data_object) <- c(attributes(data_object), config[attrs_to_add])

  attr(data_object, "dp_name") <- dpbuild::dpname_get(data_object = data_object)
  class(data_object) <- c("dp", class(data_object))

  return(data_object)
}

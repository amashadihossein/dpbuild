#' @title Converts a data product to a link
#' @description Given the name and version of a data product, it returns a 
#' custom function for retrieving that data product
#' @param dp_name name of the data product
#' @param dp_version version of the data product
#' @examples  \dontrun{
#' dp_cars_lnk <- dp_tolink(dp_name = "dp-cars-us001",dp_version = "4dc379a")
#' }
#' @export

dp_tolink <- function(dp_name, dp_version){
  
  # TODO: this function to be consolidated with dpinput_read so to provide the 
  # user a single interface for building the input object into the data product 
  # which could be table links or data products links or a combo
  
  force(dp_name)
  force(dp_version)
  
  lnk <- function(board_params = NULL, creds = NULL, ...){
    args <- list(...)
    if(!is.null(args$conf) & is.null(args$config))
      args$config <- args$conf
    
    
    if(length(board_params) == 0 | length(creds) == 0){
      if(!is.null(args$config)){
        
        if(length(board_params) > 0 & length(creds) == 0)
          warning(cli::format_warning(glue::glue("creds is empty! board_params is", 
                                                 " ignored. will attemp to use",
                                                 " config")))
        if(length(board_params) == 0 & length(creds) > 0)
          warning(cli::format_warning(glue::glue("board_params is empty! creds is", 
                                                 " ignored. will attemp to use",
                                                 " config")))
        
        board_params <- args$config$board_params
        creds <- args$config$creds
        if(length(board_params) == 0 | length(creds) == 0)
          stop(cli::format_error(glue::glue("Provide either a parameter-set",
                                            " board_params and creds or a", 
                                            " valid config parameter named config"
          )))
      }else{
        stop(cli::format_error(glue::glue("Provide either a parameter-set",
                                          " board_params and creds or a", 
                                          " valid config parameter named config"))
        )
      }
    }
    
    dpi::dp_connect(board_params = board_params, creds = creds)
    dp <- dpi::dp_get(board_params = board_params, data_name = dp_name,
                      version = dp_version)
    return(dp)
  }
  
  invisible(lnk)
}
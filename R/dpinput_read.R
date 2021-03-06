#' @title Read data product input manifest
#' @description Reads yaml DaaP input manifest and returns a structured pinlink
#' @param daap_input_yaml daap_input yaml file imported
#' @param add_metadata T/F when TRUE dpinput format will include metadata
#' @return daap_input as a structured list of anonymous functions each when 
#' called retrieves the specific data
#' @export
dpinput_read <- 
  function(daap_input_yaml = yaml::read_yaml(file =  "./.daap/daap_input.yaml"),
           add_metadata = F){
  
  dpinput <- purrr::map(daap_input_yaml, .f = function(x){
    
    if(is.list(x)){
      
      if(all(c("id","name","pin_version","synced") %in% names(x))){
        metadata <- as.data.frame(x)
        pinlink <- make_pinlink(list(metadata = metadata))
        
        if(! add_metadata)
          return(pinlink)
        
        dpinput <- list(data = pinlink, metadata = metadata)
        class(dpinput) <- "dpinput"
        
        return(dpinput)
      }else{
        return(dpinput_read(x, add_metadata = add_metadata))
      }
      
    }
  })
  
  return(dpinput)
}



#' @title Make Pinlink
#' @description Converts a synced input into a function that serves as a link.
#' @param synced_input_i a single pin synced data
#' @return a function that receives config and returns the specific data
#' @keywords internal

make_pinlink <- function(synced_input_i){

  if(!synced_input_i$metadata$synced)
    return(NULL)
  
  data_name <- synced_input_i$metadata$name
  data_version <- synced_input_i$metadata$pin_version
  data_class <- synced_input_i$metadata$class
  
  cast_class <- identity
  if(data_class[1] == "data.frame")
    cast_class <- as.data.frame
  if(data_class[1] == "tbl_df")
    cast_class <- tibble::as.tibble


  function(board_params = NULL, creds = NULL, ...){
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
    
    board_params$board_alias <- paste0(board_params$board_alias,"_dpinput")
    dpi::dp_connect(board_params = board_params, creds = creds, 
                    board_subdir = "dpinput")
    
    dpinput_i <- dpi::dp_get(board_params = board_params, data_name = data_name,
                             version = data_version)
    dpinput_i <- cast_class(dpinput_i)
    
    return(dpinput_i)
  }

}

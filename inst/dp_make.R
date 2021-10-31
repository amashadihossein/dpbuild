#'==============================================================================
#' This is the main task manager assembling data by calling tasks as functions 
#' within R sub-directory
#'  
#' Pre-requisite set up
#' 1- project set up with dp_init
#' 2- input files syncd
#' 3- Remote creds and params in environment: Ex. 
#' Sys.setenv("AWS_KEY" = keyring::key_get(service = "AWS_KEY", 
#'                               username = "myusername",
#'                               keyring = "mykeyringname"))
#' Sys.setenv("AWS_SECRET" = keyring::key_get(service = "AWS_SECRET",
#'                               username = "myusername",
#'                               keyring = "mykeyringname"))
#' Sys.setenv(GITHUB_PAT = keyring::key_get(service = "GITHUB_PAT",
#'                               username = "myusername",
#'                               keyring = "mykeyringname"))
#' 4- renv is to be restored: Ex. renv::restore()
#'==============================================================================

options(stringsAsFactors = F)
R.utils::sourceDirectory("R")

config <- dpconf_get(project_path = "./")

conflict_prefer("filter", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("arrange", "dplyr")


daap_plan <- drake_plan(

  # Initial Set up
  data_files_read = target(
    command =  dpbuild::dpinput_read(),
    trigger = trigger(change = file_in("./.daap/daap_input.yaml"))),

  # Derive datatopic1
  # datatopic1 = derive_datatopic1(clin = data_files_read, config = config),


  # Structure data obj
  data_object = 
    dp_structure(data_files_read, config, output = list(), metadata = list()),

  # Structure output and add metadata
  data_is_written = 
    dpbuild::dp_write(data_object = data_object, project_path = ".")

  # Run data test
  #' TODO: data_is_written_out ensures .RDS can be hashed. We may only need to 
  #'       hash data_obj itself rather than readRDS
  # data_is_tested =
  #   unit_test_data(d = data_obj, 
  #                  data_is_written_out = data_is_written_and_committed)
)

make(daap_plan, lock_envir = FALSE)

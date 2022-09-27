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
R.utils::sourceDirectory("R", modifiedOnly = F)

config <- dpconf_get(project_path = "./")

conflict_prefer("filter", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("arrange", "dplyr")

tarchetypes::tar_plan(
  list(
    tar_target(
      name = data_files_read,
      command =  dpbuild::dpinput_read(),
      cue = tar_cue(mode = "thorough", command = TRUE, depend = TRUE)
    ),

    tar_target(
      name = data_object,
      command =  dp_structure(data_files_read, config, output = list(), 
                              metadata = list())
    ),

    tar_target(
      name = data_is_written,
      command = dpbuild::dp_write(data_object = data_object, project_path = ".")
      )
    )
)

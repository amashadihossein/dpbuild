#' @importFrom dpi board_params_set_s3
#' @export
dpi::board_params_set_s3

#' @importFrom dpi board_params_set_labkey
#' @export
dpi::board_params_set_labkey

#' @importFrom dpi creds_set_aws
#' @export
dpi::creds_set_aws

#' @importFrom dpi creds_set_labkey
#' @export
dpi::creds_set_labkey

#' @title Initialize data product project
#' @description Initializes a data product project
#' @param project_path path to the project folder. The folder name will be used
#' as project name. If the path doesn't exist it will be created.
#' @param project_description A high level description of the project. Example:
#' integrated, clinical and translational data from study x.
#' @param branch_name An abbreviation to capture the specific reason for which
#' data was processed. Example m3cut (as in month 3 data cut)
#' @param branch_description A high level description of the branch
#' @param readme_general_note Optional general note which will be added as
#' metadata to the data object
#' @param board_params_set_dried Character representation of the function for
#' setting board_params. Use `fn_dry()` in combination with
#' `dpi::board_params_set_s3` or `dpi::board_params_set_labkey`. See example.
#' @param creds_set_dried Character representation of the function for setting
#' creds. Use `fn_dry()` in combination with `dpi::creds_set_aws` or
#' `dpi::creds_set_labkey`. See example
#' @param github_repo_url the https url for the github repo
#' @param git_ignore a character vector of the files and directories to be
#' ignored by git.
#' @param ... any other metadata to be captured in the config file
#' @return project path
#' @examples  \dontrun{
#' # Dry function call to setting board_params
#' board_params_set_dried <-
#' fn_dry(dpi::board_params_set_labkey(board_alias = "labkey",
#'  url = 'https:url_to_labkey/labkey',
#'  folder = 'project_folder/subfolder'))
#'
#' # Dry function call to setting credentials
#' creds_set_dried <-
#' fn_dry(dpi::creds_set_labkey(api_key = Sys.getenv("LABKEY_API_KEY")))
#'
#' # Initialize dp repo
#' dp_repo <- dp_init(project_path = "dp_test",
#'                 project_description = "Test data product",
#'                 branch_name = "us001",
#'                 branch_description = "User story 1",
#'                 commit_description = "First dp commit",
#'                 readme_general_note = "This data object is generated for testing purposes",
#'                 board_params_set_dried = board_params_set_dried,
#'                 creds_set_dried = creds_set_dried,
#'                 github_repo_url = "https://github.com/teamAccount/me/dp_test.git")
#' }
#' @export
dp_init <- function(project_path = fs::path_wd(),
                    project_description,
                    branch_name,
                    branch_description,
                    readme_general_note = character(0),
                    board_params_set_dried,
                    creds_set_dried,
                    github_repo_url,
                    git_ignore = c(".drake/", "_targets/", "input_files/",
                                   "output_files/", ".Rprofile", ".Renviron",
                                   ".Rhistory", ".Rproj.user", ".Rproj.user/",
                                   ".DS_Store", "*.csv", "*.tsv", "*.rds",
                                   "*.sas7bdat"),
                    ...){

  commit_description <- "dp init"
  wd0 <- fs::path_wd()

  if(fs::dir_exists(project_path)){
    stop(cli::format_error("{project_path} folder already exists in the current directory."))
  }

  if (is_valid_dp_repository(project_path)){
    stop(cli::format_error("You are already inside a dp repository."))
  }

  if(!fs::dir_exists(path = project_path))
    fs::dir_create(project_path)


  if(length(fs::dir_ls(path = project_path))!=0)
    stop(cli::format_error(glue::glue("There is already a non-empty directory ",
                                      "{basename(project_path)} ! If starting a ",
                                      "new project run dp_init where ",
                                      "{basename(project_path)} does not exist ",
                                      "or is empty!")))


  project_name <- basename(path = project_path)
  repo <- dp_git_init(project_path = project_path, project_name = project_name,
                      branch_name = branch_name,
                      github_repo_url = github_repo_url,
                      board_params_set_dried = board_params_set_dried,
                      creds_set_dried = creds_set_dried,
                      git_ignore = git_ignore)
  last_commit <- git2r::last_commit(repo = repo)


  if(!fs::dir_exists(path = glue::glue("{project_path}/.daap")))
    fs::dir_create(glue::glue("{project_path}/.daap"))

  if(!fs::dir_exists(path = glue::glue("{project_path}/input_files")))
    fs::dir_create(glue::glue("{project_path}/input_files"))

  if(!fs::dir_exists(path = glue::glue("{project_path}/output_files")))
    fs::dir_create(glue::glue("{project_path}/output_files"))

  dpconf <- dpconf_init(project_path = project_path,
                        project_name = project_name,
                        project_description = project_description,
                        branch_name = branch_name,
                        branch_description = branch_description,
                        readme_general_note = readme_general_note,
                        board_params_set_dried = board_params_set_dried,
                        creds_set_dried = creds_set_dried, ...)

  if(!fs::dir_exists(fs::path_tidy(glue::glue("{project_path}/R"))))
    fs::dir_create(fs::path_tidy(glue::glue("{project_path}/R")))

  fs::file_copy(path = system.file("global.R", package = "dpbuild"),
                new_path = fs::path_tidy(glue::glue("{project_path}/R")),
                overwrite = T)

  fs::file_copy(path = system.file(".renvignore", package = "dpbuild"),
                new_path = project_path, overwrite = T)
  # add renv
  renv::init(project = fs::path_tidy(project_path), restart = F)
  setwd(wd0)

  # commit git
  repo <- git2r::repository(path = fs::path_tidy(project_path))
  add_these <- unlist(git2r::status(repo = repo))
  git2r::add(repo = repo, path = glue::glue("{project_path}/{add_these}") )
  git2r::commit(repo = repo, all = TRUE, message = commit_description)


  return(fs::path_dir(repo$path))

}



#' @title Initialize daap configuration file
#' @description Initializes daap configuration file
#' @param project_path path to the project folder
#' @param project_name the name of the project. This is typically the name of
#' the folder where the project is set
#' @param project_description A high level description of the project. Example:
#' integrated, clinical and translational data from study x.
#' @param branch_name An abbreviation to capture the specific reason for which
#' data was processed. Example m3cut (as in month 3 data cut)
#' @param branch_description A high level description of the branch
#' @param readme_general_note Optional general note which will be added as
#' metadata to the data object
#' @param board_params_set_dried Character representation of the function for
#' setting board_params. Use `fn_dry()`
#' in combination with `dpi::board_params_set_s3` or
#' `dpi::board_params_set_labkey`. See example.
#' @param creds_set_dried Character representation of the function for setting
#' creds. Use `fn_dry()` in combination with
#' `dpi::creds_set_aws` or `dpi::creds_set_labkey`. See example
#' @param ... any other metadata to be captured in the config file
#' @return dpconf
#' @keywords internal

dpconf_init <- function(project_path,
                        project_name,
                        project_description = character(0),
                        branch_name,
                        branch_description = character(0),
                        readme_general_note = character(0),
                        board_params_set_dried,
                        creds_set_dried,
                        ...){

  if(!fs::dir_exists(path = glue::glue("{project_path}/.daap")))
    fs::dir_create(glue::glue("{project_path}/.daap"))


  dpconf <- c(list(project_path = project_path,
                   project_name = project_name,
                   project_description = project_description,
                   branch_name = branch_name,
                   branch_description = branch_description,
                   readme_general_note = readme_general_note,
                   board_params_set_dried = board_params_set_dried,
                   creds_set_dried = creds_set_dried),
              list(...))

  dpconf <- dpconf_write(project_path = project_path, dpconf = dpconf)

  return(dpconf)

}


#' @title Dry a called function
#' @description Make a text representation of a called function call
#' @param fn_called a function called
#' @return character representation of the called function
#' @examples \dontrun{
#' fn_dry(sum(log(1:10)))
#' }
#' @export
fn_dry <- function(fn_called){
  fn_as_call <- rlang::enexpr(fn_called)
  dried_fn <- rlang::expr_deparse(fn_as_call, width = 300) # large width avoids line wrapping
  if (length(dried_fn) > 1) {
    warning("input expression is too long; line wrapping created")
  }
  return(dried_fn)
}


#' @title Hydrate a dried called function
#' @description execute and returns the value of function call given its textual
#'  (dried) representation
#' @param fn_called a function called
#' @return value of the called function given its textual representation
#' @examples \dontrun{
#' fn_hydrate(fn_dry(sum(log(1:10))))
#' }
#' @keywords internal

fn_hydrate <- function(dried_fn){
  return(eval(rlang::parse_expr(dried_fn)))
}


#' @title dp git initialization
#' @description Initializes the git repo according to the requirements of dp
#' @param project_path Path to the project folder
#' @param project_name The name of the project. This is typically the name of
#' the folder where the project is set
#' @param branch_name An abbreviation to capture the specific reason for which
#' data was processed. Example m3cut (as in month 3 data cut)
#' @param board_params_set_dried Character representation of the function for
#' setting board_params. Use `fn_dry()` in combination with
#' `dpi::board_params_set_s3` or `dpi::board_params_set_labkey`. See example.
#' @param creds_set_dried Character representation of the function for setting
#' creds. Use `fn_dry()` in combination with `dpi::creds_set_aws` or
#' `dpi::creds_set_labkey`. See example
#' @param github_repo_url the https url for the github repo
#' @param git_ignore A character vector of the files and directories to be
#' ignored by git.
#' @keywords internal
dp_git_init <- function(project_path, project_name, branch_name,
                        github_repo_url,
                        board_params_set_dried,
                        creds_set_dried,
                        git_ignore){

  if(length(fs::dir_ls(path = project_path))!=0)
    stop(cli::format_error(glue::glue("There is already a non-empty directory ",
                                      "{basename(project_path)} ! If starting a ",
                                      "new project run dp_init where ",
                                      "{basename(project_path)} does not exist ",
                                      "or is empty!")))


  repo <- git2r::init(path = project_path)
  git_status <- git2r::status(repo = repo, ignored = T)
  repo_is_clean <- all(sapply(git_status,length)==0)
  git2r::remote_add(repo = repo, name = "origin", url = github_repo_url)

  git_conf <- git2r::config(repo = repo)$global

  if(length(git_conf$user.name) == 0)
    stop(cli::format_error(glue::glue("git username not configured. Set git ",
                                      "username by git2r::config(global = T, ",
                                      "user.name = \"<YOUR_USER_NAME>\")")))

  if(length(git_conf$user.email) == 0)
    stop(cli::format_error(glue::glue("git user.email not configured. Set git ",
                                      "user.email by git2r::config(global = T, ",
                                      "user.email = \"<YOUR_EMAIL>\")")))

  if(repo_is_clean){

    writeLines(glue::glue_collapse({git_ignore},sep = "\n"),
               file.path(project_path, ".gitignore"))
    git2r::add(repo = repo, path = glue::glue("{project_path}/.gitignore"))

    add_readme(project_path = project_path,
               dp_title = glue::glue("Data Product {project_name}_{branch_name}"),
               github_repo_url = github_repo_url,
               board_params_set_dried = board_params_set_dried,
               creds_set_dried = creds_set_dried)

    git2r::add(repo = repo, path = glue::glue("{project_path}/README.md"))

    commit_1 <- git2r::commit(repo, message =  "project init")

    # Create a branch
    branch_1 <- git2r::branch_create(commit_1, name = branch_name)

    # change branch
    git2r::checkout(object = repo, branch = branch_name)

    # dpconf$branch_name <- git2r::repository_head(repo = repo)$name

    # last_commit <- git2r::last_commit(repo = repo)
  }else{
    stop(cli::format_error(glue::glue("Repo is not clean. dp_git is to be used ",
                                      "with a clean repo. Either clean the repo ",
                                      "or use git directly for git initiation")))
  }

  return(repo)
}

#' @title Add readme to the project
#' @param project_path Path to the project folder
#' @param dp_title readme title
#' @param board_params_set_dried Character representation of the function for
#' setting board_params. Use `fn_dry()` in combination with
#' `dpi::board_params_set_s3` or `dpi::board_params_set_labkey`. See example.
#' @param creds_set_dried Character representation of the function for setting
#' creds. Use `fn_dry()` in combination with `dpi::creds_set_aws` or
#' `dpi::creds_set_labkey`. See example
#' @param github_repo_url github repo url
#' @keywords internal
add_readme <- function(project_path, dp_title, github_repo_url,
                       board_params_set_dried, creds_set_dried){

  flname <- flname_xos_get(fl = "README.RMD")
  fs::file_copy(path = system.file(flname, package = "dpbuild"),
                new_path = project_path, overwrite = T)

  board_params_set<- fn_hydrate(board_params_set_dried)

  rendered <- try(rmarkdown::render(
    input = glue::glue("{project_path}/{flname}"),
    params =  list(dp_title = dp_title, github_repo_url = github_repo_url,
                   board_params_set = board_params_set,
                   creds_set_dried = creds_set_dried)))

  if("try-error" %in% class(rendered))
    writeLines(glue::glue("## {dp_title}"),
               file.path(project_path, "README.md"))
}



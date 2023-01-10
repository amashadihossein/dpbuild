#' @title R package version update
#' @description Modifies R package records of `renv` for this project
#' @details  This is primarily intended to enable updating package that are
#' not on CRAN or GitHub (e.g. RSPM packages). `renv::update` can update when
#' packages are on CRAN or GitHub. However as core packages of `DaaPR` are
#' internal, this helper function is currently needed. Note, currently update
#' to packages from git are not supported. To update DaaPR elements, set install
#' options to RSPM `options(repos = <RSPM URL>)` Update DaaPR > run this
#' function to update all DaaPR packages on renv.lock > Delete `renv` folder >
#' run `renv::restore()` > Follow subsequent steps.
#' @param project_path path to the project
#' @param pkg_name name of the package to be added or updated
#' @param pkg_version pkg version, if not provided, tries to retrieve the latest
#' from RSPM
#' @param pkg_sha sha as recorded on the repository. tries to
#' retrieve the latest from RSPM
#' @param repo_name by default RSPM
#' @param repo_url url corresponding to the repo where the package is
#' @param force_repo_overwrite T/F. Use with caution. Changing this, will change
#' the url corresponding to the repo name/alias universally and may break the
#' lock file
#' @param verbose T/F
#' @return the update renv.lock JSON as a list
#' @examples \dontrun{
#' dppkg_modify(
#'   project_path = ".", pkg_name = "dpdeploy",
#'   pkg_version = "0.0.0.9008",
#'   pkg_sha = "3e9544b7ea42f683647df5a4e21238ae7194b580", verbose = T
#' )
#' }
#' @export
dppkg_modify <- function(project_path = ".",
                         pkg_name,
                         pkg_version = character(0),
                         pkg_sha = character(0),
                         repo_name = "RSPM",
                         repo_url,
                         force_repo_overwrite = F,
                         verbose = T) {
  if (length(pkg_version) == 0 | length(pkg_sha) == 0) {
    if (length(pkg_version) != 0) {
      warning(cli::format_warning(glue::glue(
        "pkg_version {pkg_version} is ",
        "ignored unless pkg_sha is also ",
        "provided"
      )))
    }

    if (length(pkg_sha) != 0) {
      warning(cli::format_warning(glue::glue(
        "pkg_sha {pkg_sha} is ignored unless ",
        "pkg_version is also provided"
      )))
    }

    pkgmeta <- try(get_rspm_pkgmeta(pkg_name = pkg_name))

    if ("try-error" %in% class(pkgmeta)) {
      stop(cli::format_error(glue::glue(
        "Failed to retrieve pkgmeta from ",
        "RSPM. Try providing pkg_version and",
        "pkg_sha in function call"
      )))
    }
    pkg_version <- pkgmeta$version
    pkg_sha <- pkgmeta$remote_sha

    if (length(pkg_version) == 0 | length(pkg_sha) == 0) {
      stop(cli::format_error(glue::glue(
        "Failed to retrieve pkgmeta from ",
        "RSPM. Try providing pkg_version and",
        "pkg_sha in function call"
      )))
    }
  }


  renv_manifest_path <- glue::glue("{project_path}/renv.lock")
  renv_manifest <- jsonlite::read_json(path = renv_manifest_path)
  repo_names_manifest <- sapply(renv_manifest$R$Repositories, function(x) x$Name)
  repo_urls_manifest <- sapply(renv_manifest$R$Repositories, function(x) x$URL)
  renv_manifest_pkgs <- names(renv_manifest$Packages)

  if (repo_name %in% repo_names_manifest) {
    reponame_index <- which(repo_names_manifest == repo_name)

    if (repo_url %in% repo_urls_manifest) {
      repo_url_i <- renv_manifest$R$Repositories[[reponame_index]]$URL
      repo_nameurl_match <- repo_url_i == repo_url


      if (!repo_nameurl_match) {
        if (force_repo_overwrite) {
          cli::cli_alert(glue::glue(
            "Overwriting repository url for repo",
            " {repo_name} from {repo_url_i} to {repo_url}"
          ))
          renv_manifest$R$Repositories[[reponame_index]]$URL <- repo_url
        } else {
          stop(cli::format_error(glue::glue(
            "URL for repo {repo_name} is ",
            "already defined! To overwrite, set ",
            "force_repo_overwrite = T"
          )))
        }
      }
    }
  } else {
    renv_manifest$R$Repositories <- c(
      renv_manifest$R$Repositories,
      list(list(Name = repo_name, URL = repo_url))
    )
    if (verbose) {
      cli::cli_alert_success(glue::glue(
        "Added repo {repo_name} with url ",
        "{repo_url}"
      ))
    }
  }




  new_pkg_record <- list()
  new_pkg_record[[pkg_name]] <- list(
    Package = pkg_name, Version = pkg_version,
    Source = "Repository",
    Repository = repo_name, RemoteSha = pkg_sha
  )
  renv_manifest$Packages[pkg_name] <- NULL
  renv_manifest$Packages <- c(renv_manifest$Packages, new_pkg_record)

  if (verbose) {
    if (pkg_name %in% renv_manifest_pkgs) {
      cli::cli_alert_success("Updated recod for package {pkg_name} in renv.lock")
    } else {
      cli::cli_alert_success("Added package {pkg_name} to renv.lock")
    }
  }

  jsonlite::write_json(
    x = renv_manifest, path = renv_manifest_path, pretty = T,
    auto_unbox = T
  )

  invisible(renv_manifest)
}

#' @title R the latest package metadata on rspm
#' @description A convenience function to hit the rspm api and get pkg metadata
#' @details  This is primarily intended to make `dppkg_modify` easier to work
#' with
#' @param pkg_name character string name of the package
#' @param rspm_api_url the api endpoint of rspm that given pkg_name retrieves
#' pkg metadata like latest version, remote_sha, etc.
#' @return pkgmeta containing `name`, `remote_sha`, `version`, `checksum`, and
#' `repository`
#' @examples \dontrun{
#' get_rspm_pkgmeta(pkg_name = "dpi")
#' }
#' @keywords internal
get_rspm_pkgmeta <- function(pkg_name,
                             rspm_api_url) {
  pm_url <-
    glue::glue("{rspm_api_url}/{pkg_name}")
  rspm_pkgmeta <- httr::GET(url = pm_url) %>% httr::content()

  return(rspm_pkgmeta[c("name", "remote_sha", "version", "checksum", "repository")])
}

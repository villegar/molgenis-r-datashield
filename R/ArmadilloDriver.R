
#' Class ArmadilloDriver with constructor armadillo
#'
#' A Armadillo DataSHIELD Service Driver implementing the DataSHIELD Interface
#' (DSI) \code{\link{DSDriver-class}}. This class should always be initialized
#' with the \code{\link{armadillo}} function.
#' It returns a singleton that allows you to connect to Armadillo
#'
#' @importClassesFrom DSI DSDriver
#' @export
#' @keywords internal
methods::setClass("ArmadilloDriver", contains = "DSDriver")

#' Create a Armadillo DataSHIELD Service driver
#'
#' Convenience function for creating a [ArmadilloDriver] object.
#'
#' @export
armadillo <- function() {
  methods::new("ArmadilloDriver")
}

#' Connect to a Armadillo DataSHIELD service
#'
#' Connect to a Armadillo DataSHIELD service, with provided credentials.
#'
#' @param drv \code{\link{ArmadilloDriver-class}} class object.
#' @param name Name of the connection, which must be unique among all the
#' DataSHIELD connections.
#' @param restore Workspace name to be restored in the newly created DataSHIELD
#' R session.
#' @param username The username to authenticate with.
#' @param password The password to authenticate with.
#' @param token Not yet used.
#' @param url URL of the server.
#' @param opts Curl options as described by httr (call httr::httr_options()
#' for details). Can be provided by "Armadillo.opts" option.
#' @param ... Unused, needed for compatibility with generic.
#'
#' @return A \code{\link{ArmadilloConnection-class}} object.
#'
#' @importMethodsFrom DSI dsConnect
#' @export
methods::setMethod(
  "dsConnect", "ArmadilloDriver",
  function(drv, name, restore = NULL, username = NULL, password = NULL,
           token = NULL, url = NULL, opts = list(), ...) {
    # Retrieve login URL and workspace name
    url_parts <- unlist(strsplit(url, "?", fixed = TRUE))
    workspace_parameters <- url_parts[2]
    root_url <- paste(url_parts[1])

    handle <- httr::handle(root_url)
    workspace_values <- stringr::str_remove_all(
      workspace_parameters,
      "workspace="
    )
    workspaces <- strsplit(workspace_values, "&", fixed = TRUE)

    # Login and load the tables of the workspace
    login_response <- httr::POST(
      handle = handle,
      path = "/login",
      encode = "form",
      body = list(username = username, password = password)
    )
    .handle_request_error(login_response)
    cookies <- httr::cookies(login_response)

    load_table_response <- httr::POST(
      handle = handle,
      path = paste0(
        "/load-tables?",
        workspace_parameters
      )
    )
    if (load_table_response$status_code == 403) {
      stop("You don't have access to one or more of the workspaces",
        call. = FALSE
      )
    }
    .handle_request_error(load_table_response)

    # Restore users workspace
    if (!is.null(restore)) {
      restore_response <- httr::POST(
        handle = handle,
        path = paste0("/load-workspace?id=", restore)
      )
      .handle_request_error(restore_response)
    }

    methods::new("ArmadilloConnection",
      name = name,
      handle = handle,
      workspaces = workspaces,
      user = username,
      cookies = cookies
    )
  }
)

#' Get driver info
#'
#' Get information about a driver.
#'
#' @param dsObj \code{\link{ArmadilloDriver-class}} class object
#' @param ... Unused, needed for compatibility with generic.
#'
#' @return The connection information. This returns the version of the
#' package (`driver.version`). There is no underlying client library.
#'
#' @importMethodsFrom DSI dsGetInfo
#' @importFrom utils packageVersion
#' @export
methods::setMethod(
  "dsGetInfo", "ArmadilloDriver",
  function(dsObj, ...) { # nolint
    return(list(
      driver.version = packageVersion("DSMolgenisArmadillo")
    ))
  }
)

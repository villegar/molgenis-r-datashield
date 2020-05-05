
#' Class MolgenisDriver with constructor Molgenis
#'
#' An Molgenis DataSHIELD Service Driver implementing the DataSHIELD Interface
#' (DSI) \code{\link{DSDriver-class}}. This class should always be initialized
#' with the \code{\link{Molgenis}} function.
#' It returns a singleton that allows you to connect to Molgenis.
#'
#' @import methods
#' @import DSI
#' @export
#' @keywords internal
setClass("MolgenisDriver", contains = "DSDriver")

#' Create a MOLGENIS DataSHIELD Service driver
#'
#' Convenient function for creating a [MolgenisDriver] object.
#'
#' @import methods
#' @import DSI
#' @export
molgenis <- function() {
  new("MolgenisDriver")
}

#' Connect to a Molgenis DataSHIELD service
#'
#' Connect to a Molgenis DataSHIELD service, with provided credentials.
#'
#' @param drv \code{\link{MolgenisDriver-class}} class object.
#' @param name Name of the connection, which must be unique among all the
#' DataSHIELD connections.
#' @param restore Workspace name to be restored in the newly created DataSHIELD
#' R session.
#' @param username
#' @param password
#' @param token
#' @param url
#' @param opts Curl options as described by httr (call httr::httr_options()
#' for details). Can be provided by "Molgenis.opts" option.
#' @param ... Unused, needed for compatibility with generic.
#'
#' @return A \code{\link{MolgenisConnection-class}} object.
#'
#' @import methods
#' @import httr
#' @export
setMethod(
  "dsConnect", "MolgenisDriver",
  function(drv, name, restore = NULL, username = NULL, password = NULL,
           token = NULL, url = NULL, opts = list(), ...) {
    # Retrieve login URL and workspace name
    url_parts <- unlist(strsplit(url, "?", fixed = TRUE))
    workspace_parameters <- url_parts[2]
    root_url <- paste(url_parts[1])

    handle <- handle(root_url)
    workspace_values <- stringr::str_remove_all(
      workspace_parameters,
      "workspace="
    )
    workspaces <- strsplit(workspace_values, "&", fixed = TRUE)

    # Login and load the tables of the workspace
    login_response <- POST(
      handle = handle,
      path = "/login",
      encode = "form",
      body = list(username = username, password = password)
    )
    .handle_request_error(login_response)
    cookies <- httr::cookies(login_response)

    load_table_response <- POST(
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
      restore_response <- POST(
        handle = handle,
        path = paste0("/load-workspace?id=", restore)
      )
      .handle_request_error(restore_response)
    }

    new("MolgenisConnection",
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
#' @param dsObj \code{\link{MolgenisDriver-class}} class object
#' @param ... Unused, needed for compatibility with generic.
#'
#' @return The connection information. This should include the version of the
#' package (`driver.version`) and the version of the underlying client
#' library (`client.version`).
#'
#' @export
methods::setMethod(
  "dsGetInfo", "MolgenisDriver",
  function(dsObj, ...) { # nolint
    return(list(
      driver.version = packageVersion("DSMolgenis")
    ))
  }
)

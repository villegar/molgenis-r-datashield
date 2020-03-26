
#' Class MolgenisDriver with constructor Molgenis
#'
#' An Molgenis DataSHIELD Service Driver implementing the DataSHIELD Interface (DSI) \code{\link{DSDriver-class}}.
#' This class should always be initialized with the \code{\link{Molgenis}} function.
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
Molgenis <- function() {
  new("MolgenisDriver")
}

#' Connect to a Molgenis DataSHIELD service
#' 
#' Connect to a Molgenis DataSHIELD service, with provided credentials.
#' 
#' @param drv \code{\link{MolgenisDriver-class}} class object.
#' @param name Name of the connection, which must be unique among all the DataSHIELD connections.
#' @param restore Workspace name to be restored in the newly created DataSHIELD R session.
#' @param username 
#' @param password 
#' @param token 
#' @param url 
#' @param opts Curl options as described by httr (call httr::httr_options() for details). Can be provided by "Molgenis.opts" option.
#' @param ... Unused, needed for compatibility with generic.
#' 
#' @return A \code{\link{MolgenisConnection-class}} object.
#' 
#' @import methods
#' @import httr
#' @export
setMethod("dsConnect", "MolgenisDriver", 
          function(drv, name, restore = NULL, username = NULL, password = NULL, token = NULL, url = NULL, opts = list(), ...) {
            #TODO implement restore parameter
            props <- list()
            props$handle <- handle(url)
            POST(handle = props$handle, path="/login", encode="form", body=list(username=username, password=password))
            connection <- new("MolgenisConnection", name = name, props = props)
            connection
          })

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
#' @import methods
#' @export
setMethod("dsGetInfo", "MolgenisDriver", function(dsObj, ...) {
  #TODO implement
})

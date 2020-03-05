#' @include MolgenisDriver.R


#' Class MolgenisConnection.
#'
#' A Molgenis connection implementing the DataSHIELD Interface (DSI) \code{\link{DSConnection-class}}.
#' 
#' @import methods
#' @import DSI
#' @export
#' @keywords internal
setClass("MolgenisConnection", contains = "DSConnection", slots = list(name = "character", props = "list"))

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
            props <- list()
            props$handle <- handle(url)
            POST(handle = props$handle, path="/login", encode="form", body=list(username=username, password=password))
            connection <- new("MolgenisConnection", name = name, props = props)
            connection
          })



#' Verify table exist and can be accessible for performing DataSHIELD operations.
#'
#' @param conn \code{\link{MolgenisConnection-class}} class object.
#' @param table The identifier of the table
#'
#' @return TRUE if table exists.
#'
#' @import methods
#' @export
setMethod("dsHasTable", "MolgenisConnection", function(conn, table) {
  props <- conn@props
  answer <- GET(handle=props$handle, path=paste0("/exists/", table))
  content(answer)
})




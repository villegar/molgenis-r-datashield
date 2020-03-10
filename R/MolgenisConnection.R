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
  response <- GET(handle=conn@props$handle, path=paste0("/exists/", table))
  content(response)
})

#' MOLGENIS asynchronous support 
#' 
#' List of DataSHIELD operations on which MOLGENIS supports asynchronicity.
#' 
#' @param conn \code{\link{MolgenisConnection-class}} class object
#' 
#' @return The named list of logicals detailing the asynchronicity support.
#' 
#' @import methods
#' @export
setMethod("dsIsAsync", "MolgenisConnection", function(conn) {
  list(aggregate = FALSE, assignTable = FALSE, assignExpr = FALSE)
})

#' Assign a table
#' 
#' Assign a MOLGENIS table in the DataSHIELD R session.
#' 
#' @param conn \code{\link{MolgenisConnection-class}} object.
#' @param symbol Name of the R symbol.
#' @param table Identifier of a table in MOLGENIS.
#' @param variables 
#' @param missings 
#' @param identifiers 
#' @param id.name 
#' @param async 
#' 
#' @return A \code{\link{MolgenisResult-class}} object.
#' 
#' @import methods
#' @export
setMethod("dsAssignTable", "MolgenisConnection", function(conn, symbol, table, variables=NULL, missings=FALSE, identifiers=NULL, id.name=NULL, async=TRUE) {
  GET(handle=conn@props$handle, path=paste0("/load/", table))
  #TODO get and assign metadata from datashield service to MolgenisResult
  new("MolgenisResult", conn = conn, rval=list(result="test"))
})


#' List methods
#'
#' List methods defined in the DataSHIELD configuration.
#'
#' @param conn \code{\link{MolgenisConnection-class}} class object
#' @param type Type of the method: "aggregate" (default) or "assign".
#'
#' @return A data frame.
#'
#' @import methods
#' @export
setMethod("dsListMethods", "MolgenisConnection", function(conn, type = "aggregate") {
  #TODO implement
  list()
})


#' Aggregate data
#'
#' Aggregate some data from the DataSHIELD R session using a valid R expression. The aggregation expression
#' must satisfy the data repository's DataSHIELD configuration.
#'
#' @param conn \code{\link{MolgenisConnection-class}} object.
#' @param expr Expression to evaluate.
#' @param async Whether the result of the call should be retrieved asynchronously. When TRUE (default) the calls are parallelized over
#'   the connections, when the connection supports that feature, with an extra overhead of requests.
#'
#' @import methods
#' @export
setMethod("dsAggregate", "MolgenisConnection", function(conn, expr, async=TRUE) {
  rawResult <- POST(handle=conn@props$handle, url=conn@props$handle.url, path="/execute/raw", body=rlang::as_string(expr), add_headers('Content-Type'='text/plain'))
  result <- unserialize(content(rawResult))
  new("MolgenisResult", conn = conn, rval=list(result=result))
})

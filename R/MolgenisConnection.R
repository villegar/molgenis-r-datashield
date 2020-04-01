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


#' Disconnect from a MOLGENIS DatasSHIELD Service
#' 
#' Disconnect from a MOLGENIS DataSHIELD Service and release all R resources. If a workspace ID is provided, the DataSHIELD
#' R session will be saved before being destroyed.
#' 
#' @param conn \code{\link{MolgenisConnection-class}} class object
#' @param save Save the DataSHIELD R session with provided ID (must be a character string).
#' 
#' @import methods
#' @export
setMethod("dsDisconnect", "MolgenisConnection", function(conn, save = NULL) {
  #TODO implement
})

#' List MOLGENIS DataSHIELD Service tables 
#' 
#' List MOLGENIS DataSHIELD Service tables that may be accessible for performing DataSHIELD operations.
#' 
#' @param conn \code{\link{MolgenisConnection-class}} class object
#' 
#' @return The fully qualified names of the tables.
#' 
#' @import methods
#' @export
setMethod("dsListTables", "MolgenisConnection", function(conn) {
  #TODO implement
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

#' MOLGENIS DataShield Service asynchronous support 
#' 
#' List of DataSHIELD operations on which MOLGENIS DataSHIELD Service supports asynchronicity.
#' 
#' When a \code{\link{DSResult-class}} object is returned on aggregation or assignment operation,
#' the raw result can be accessed asynchronously, allowing parallelization of DataSHIELD calls
#' over multpile servers. The returned named list of logicals will specify if asynchronicity is supported for:
#' aggregation operation ('aggregate'), table assignment operation ('assignTable'),
#' expression assignment operation ('assignExpr').
#' @param conn \code{\link{MolgenisConnection-class}} class object
#' 
#' @return The named list of logicals detailing the asynchronicity support.
#' 
#' @import methods
#' @export
setMethod("dsIsAsync", "MolgenisConnection", function(conn) {
  list(aggregate = TRUE, assignTable = FALSE, assignExpr = FALSE)
})

#' List R symbols
#' 
#' List symbols living in the DataSHIELD R session.
#' 
#' @param conn \code{\link{MolgenisConnection-class}} class object
#' 
#' @return A character vector.
#' 
#' @import opalr
#' @import methods
#' @export
setMethod("dsListSymbols", "MolgenisConnection", function(conn) {
  #TODO implement
})

#' Remove an R symbol
#' 
#' Remove a symbol living in the DataSHIELD R session. After removal, the data identified by the symbol 
#' will not be accessible in the DataSHIELD R session on the server side.
#' 
#' @param conn \code{\link{MolgenisConnection-class}} class object
#' @param symbol Name of the R symbol.
#' 
#' @import methods
#' @export
setMethod("dsRmSymbol", "MolgenisConnection", function(conn, symbol) {
  #TODO implement
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
setMethod("dsAssignTable", "MolgenisConnection", function(conn, symbol, table, variables=NULL, missings=FALSE, identifiers=NULL, id.name=NULL, async=FALSE) {
  GET(handle=conn@props$handle, path=paste0("/load/", table, "/", symbol))
  #TODO get and assign metadata from datashield service to MolgenisResult
  new("MolgenisResult", conn = conn, rval=list(result="test", async = async))
})


#' List methods
#'
#' List methods defined in the DataSHIELD configuration.
#'
#' @param conn \code{\link{MolgenisConnection-class}} class object
#' @param type Type of the method: "aggregate" (default) or "assign".
#'
#' @return A data.frame with columns: name, type ('aggregate' or 'assign'), class ('function' or 'script'), value, package, version.
#'
#' @import methods
#' @export
setMethod("dsListMethods", "MolgenisConnection", function(conn, type = "aggregate") {
  #TODO implement
  list()
})

#' List packages
#' 
#' List packages with their versions defined in the DataSHIELD configuration.
#' 
#' @param conn \code{\link{MolgenisConnection-class}} class object
#' 
#' @return A data.frame with columns: name, version.
#' 
#' @import methods
#' @export
setMethod("dsListPackages", "MolgenisConnection", function(conn) {
  #TODO implement
})

#' List workspaces
#' 
#' List workspaces saved in the data repository.
#' 
#' @param conn \code{\link{MolgenisConnection-class}} class object
#' 
#' @return A data frame.
#' 
#' @import methods
#' @export
setMethod("dsListWorkspaces", "MolgenisConnection", function(conn) {
  #TODO implement
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
  rawResult <- POST(handle=conn@props$handle,
                    url=conn@props$handle$url,
                    query=list(async = async),
                    path="/execute",
                    body=rlang::as_string(expr),
                    add_headers('Content-Type'='text/plain',
                                'Accept'='application/octet-stream'))
  if (async) {
    result <- NULL
  } else {
    result <- unserialize(content(rawResult))
  }
  new("MolgenisResult", conn = conn, rval=list(result=result, async=async))
})


#' Get connection info
#' 
#' Get information about a connection.
#' 
#' @param dsObj \code{\link{MolgenisConnection-class}} class object
#' @param ... Unused, needed for compatibility with generic.
#' 
#' @return The connection information. This should report the version of
#' the data repository application (`repo.version`) and its name (`repo.name`),
#' the database name (`dbname`), username, (`username`), host (`host`), port (`port`), etc.
#' It MAY also include any other arguments related to the connection
#' (e.g., thread id, socket or TCP connection type). It MUST NOT include the
#' password.
#' 
#' @import methods
#' @export
setMethod("dsGetInfo", "MolgenisConnection", function(dsObj, ...) {
  #TODO implement
})
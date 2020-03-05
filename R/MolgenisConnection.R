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

#' MOLGENIS asynchronous support 
#' 
#' List of DataSHIELD operations on which MOLGENIS supports asynchronicity.
#' 
#' @param conn \code{\link{OpalConnection-class}} class object
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
#' @param variables List of variable names or Javascript expression that selects the variables of a table (ignored if value does not refere to a table). See javascript documentation: http://wiki.obiba.org/display/OPALDOC/Variable+Methods
#' @param missings If TRUE, missing values will be pushed from Opal to R, default is FALSE. Ignored if value is an R expression.
#' @param identifiers Name of the identifiers mapping to use when assigning entities to R (from Opal 2.0).
#' @param id.name Name of the column that will contain the entity identifiers. If not specified, the identifiers
#'   will be the data frame row names. When specified this column can be used to perform joins between data frames.
#' @param async Whether the result of the call should be retrieved asynchronously. When TRUE (default) the calls are parallelized over
#'   the connections, when the connection supports that feature, with an extra overhead of requests.
#' 
#' @return A \code{\link{OpalResult-class}} object.
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
  result <- POST(handle=conn@props$handle, url=conn@props$handle.url, path="/execute/", body=rlang::as_string(expr), add_headers('Content-Type'='text/plain'))
  new("MolgenisResult", conn = conn, rval=list(result=result))
})

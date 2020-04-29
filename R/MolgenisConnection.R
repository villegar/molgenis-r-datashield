#' @include MolgenisDriver.R

setOldClass("handle")

#' Class MolgenisConnection.
#'
#' A Molgenis connection implementing the DataSHIELD Interface (DSI)
#' \code{\link{DSConnection-class}}.
#'
#' @import methods
#' @import DSI
#' @export
#' @keywords internal
setClass("MolgenisConnection",
         contains = "DSConnection",
         slots = list(name = "character",
                      workspaces = "list",
                      handle = "handle",
                      user = "character"))


#' Disconnect from a MOLGENIS DatasSHIELD Service
#'
#' Disconnect from a MOLGENIS DataSHIELD Service and release all R resources. If
#' a workspace ID is provided, the DataSHIELD R session will be saved before
#' being destroyed.
#'
#' @param conn \code{\link{MolgenisConnection-class}} class object
#' @param save Save the DataSHIELD R session with provided ID (must be a
#' character string).
#'
#' @import methods
#' @export
setMethod("dsDisconnect", "MolgenisConnection", function(conn, save = NULL) {
  if (!is.null(save)) {
    response <- POST(handle = conn@handle, path = paste0("/workspaces/", save))
    .handle_request_error(response)
  }
  POST(handle = conn@handle, path = "/logout")
})

#' List MOLGENIS DataSHIELD Service tables
#'
#' List MOLGENIS DataSHIELD Service tables that may be accessible for performing
#' DataSHIELD operations.
#'
#' @param conn \code{\link{MolgenisConnection-class}} class object
#'
#' @return The fully qualified names of the tables.
#'
#' @import methods
#' @export
setMethod("dsListTables", "MolgenisConnection", function(conn) {
  response <- GET(handle = conn@handle, path = paste0("/tables"))
  .handle_request_error(response)
  .unlist_character_list(content(response))
})

#' Verify table exist and can be accessible for performing DataSHIELD
#' operations.
#'
#' @param conn \code{\link{MolgenisConnection-class}} class object.
#' @param table The identifier of the table
#'
#' @return TRUE if table exists.
#'
#' @import methods
#' @export
setMethod("dsHasTable", "MolgenisConnection", function(conn, table) {
  response <- HEAD(handle = conn@handle, path = paste0("/tables/", table))
  .handle_request_error(response)

  response$status_code == 200
})

#' MOLGENIS DataShield Service asynchronous support
#'
#' List of DataSHIELD operations on which MOLGENIS DataSHIELD Service supports
#' asynchronicity.
#'
#' When a \code{\link{DSResult-class}} object is returned on aggregation or
#' assignment operation, the raw result can be accessed asynchronously, allowing
#' parallelization of DataSHIELD calls over multpile servers. The returned named
#' list of logicals will specify if asynchronicity is supported for:
#' aggregation operation ('aggregate'), table assignment operation
#' ('assignTable'), expression assignment operation ('assignExpr').
#' @param conn \code{\link{MolgenisConnection-class}} class object
#'
#' @return The named list of logicals detailing the asynchronicity support.
#'
#' @import methods
#' @export
setMethod("dsIsAsync", "MolgenisConnection", function(conn) {
  list(aggregate = TRUE, assignTable = FALSE, assignExpr = TRUE)
})

#' List R symbols
#'
#' List symbols living in the DataSHIELD R session.
#'
#' @param conn \code{\link{MolgenisConnection-class}} class object
#'
#' @return A character vector.
#'
#' @import methods
#' @export
setMethod("dsListSymbols", "MolgenisConnection", function(conn) {
  response <- GET(handle = conn@handle, path = "/symbols")
  .handle_request_error(response)
  .unlist_character_list(content(response))
})

#' Remove an R symbol
#'
#' Remove a symbol living in the DataSHIELD R session. After removal, the data
#' identified by the symbol will not be accessible in the DataSHIELD R session
#' on the server side.
#'
#' @param conn \code{\link{MolgenisConnection-class}} class object
#' @param symbol Name of the R symbol.
#'
#' @import methods
#' @export
setMethod("dsRmSymbol", "MolgenisConnection", function(conn, symbol) {
  response <- DELETE(handle = conn@handle, path = paste0("/symbols/", symbol))
  .handle_request_error(response)
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
setMethod("dsAssignTable", "MolgenisConnection",
          function(conn, symbol, table, variables=NULL, missings=FALSE,
                   identifiers=NULL, id.name=NULL, async=TRUE) { # nolint
  response <- POST(handle = conn@handle,
                   path = paste0("/symbols/", symbol, "?table=", table))
  .handle_request_error(response)

  #TODO need to return something like this
  # Check Opal code:
  # Response.created(getSymbolURI(uri)).entity(id)
  # .type(MediaType.TEXT_PLAIN_TYPE).build(); as a result

  if (async) {
    result <- NULL
  }else{
    result <- .retry_until_last_result(conn)
  }

  new("MolgenisResult",
      conn = conn,
      rval = list(result = result, async = async))
})


#' List methods
#'
#' List methods defined in the DataSHIELD configuration.
#'
#' @param conn \code{\link{MolgenisConnection-class}} class object
#' @param type Type of the method: "aggregate" (default) or "assign".
#'
#' @return A data.frame with columns: name, type ('aggregate' or 'assign'),
#' class ('function' or 'script'), value, package, version.
#'
#' @import methods
#' @export
setMethod("dsListMethods", "MolgenisConnection",
          function(conn, type = "aggregate") {
  response <- GET(handle = conn@handle,
                  url = conn@handle$url,
                  path = paste0("/methods?type=", toupper(type)),
                  add_headers("Accept' = 'application/json"))
  .handle_request_error(response)

  df <- .list_to_data_frame(content(response))
  .fill_column(df, "type", type)
  .fill_column(df, "class", "function")
  .rename_column(df, "function", "value")
  df
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
  response <- GET(handle = conn@handle,
                  url = conn@handle$url,
                  path = "/packages",
                  add_headers("Accept" = "application/json"))
  .handle_request_error(response)

  extracted_cols <- lapply(content(response), function(x) c(x$name, x$version))
  .list_to_data_frame(extracted_cols)
})

#' List workspaces
#'
#' List workspaces saved in the data repository.
#'
#' @param conn \code{\link{MolgenisConnection-class}} class object
#'
#' @return A data.frame with columns: name, lastAccessDate, size, user.
#'
#' @import methods
#' @export
setMethod("dsListWorkspaces", "MolgenisConnection", function(conn) {
  response <- GET(handle = conn@handle,
                  url = conn@handle$url,
                  path = "/workspaces",
                  add_headers("Accept" = "application/json"))
  .handle_request_error(response)

  df <- .list_to_data_frame(content(response))
  .fill_column(df, "user", conn@user)
  .rename_column(df, "lastModified", "lastAccessDate")
  df
})

#' Save workspace
#'
#' Save workspace on the data repository.
#'
#' @param conn \code{\link{MolgenisConnection-class}} class object
#' @param name Name of the workspace.
#'
#' @import methods
#' @export
setMethod("dsSaveWorkspace", "MolgenisConnection", function(conn, name) {
  response <- POST(handle = conn@handle, path = paste0("/workspaces/", name))
  .handle_request_error(response)
})

#' Remove a workspace
#'
#' Remove a workspace on the data repository.
#'
#' @param conn \code{\link{MolgenisConnection-class}} class object
#' @param name Name of the workspace.
#'
#' @import methods
#' @export
setMethod("dsRmWorkspace", "MolgenisConnection", function(conn, name) {
  response <- DELETE(handle = conn@handle, path = paste0("/workspaces/", name))
  .handle_request_error(response)
})

#' Assign the result of an expression
#'
#' Assign a result of the execution of an expression in the DataSHIELD R
#' session.
#'
#' @param conn \code{\link{MolgenisConnection-class}} object.
#' @param symbol Name of the R symbol.
#' @param expr A R expression with allowed assign functions calls.
#' @param async Whether the result of the call should be retrieved
#' asynchronously. When TRUE (default) the calls are parallelized over the
#' connections, when the connection supports that feature, with an extra
#' overhead of requests.
#'
#' @return A \code{\link{MolgenisResult-class}} object.
#'
#' @import methods
#' @export
setMethod("dsAssignExpr", "MolgenisConnection",
          function(conn, symbol, expr, async = TRUE) {
  response <- POST(handle = conn@handle,
                    url = conn@handle$url,
                    query = list(async = async),
                    path = paste0("/symbols/", symbol),
                    body = .deparse(expr),
                    add_headers("Content-Type" = "text/plain"))

  .handle_request_error(response)

  if (async) {
    result <- NULL
  } else {
    result <- .retry_until_last_result(conn)
  }

  new("MolgenisResult", conn = conn, rval = list(result = NULL, async = async))
})

#' Aggregate data
#'
#' Aggregate some data from the DataSHIELD R session using a valid R expression.
#' The aggregation expression must satisfy the data repository's DataSHIELD
#' configuration.
#'
#' @param conn \code{\link{MolgenisConnection-class}} object.
#' @param expr Expression to evaluate.
#' @param async Whether the result of the call should be retrieved
#' asynchronously. When TRUE (default) the calls are parallelized over the
#' connections, when the connection supports that feature, with an extra
#' overhead of requests.
#'
#' @import methods
#' @export
setMethod("dsAggregate", "MolgenisConnection",
          function(conn, expr, async = TRUE) {
  response <- POST(handle = conn@handle,
                   url = conn@handle$url,
                   query = list(async = async),
                   path = "/execute",
                   body = .deparse(expr),
                   add_headers("Content-Type" = "text/plain",
                               "Accept" =
                                 "application/octet-stream,application/json"))
  print(response)
  .handle_request_error(response)

  if (async) {
    result <- NULL
  } else {
    if (response$status_code == 500) {
      .handle_last_command_error(conn@handle)
    }

    result <- unserialize(content(response))
  }
  new("MolgenisResult",
      conn = conn,
      rval = list(result = result, async = async))
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
#' the database name (`dbname`), username, (`username`), host (`host`), port
#' (`port`), etc.
#' It MAY also include any other arguments related to the connection
#' (e.g., thread id, socket or TCP connection type). It MUST NOT include the
#' password.
#'
#' @import methods
#' @export
setMethod("dsGetInfo", "MolgenisConnection", function(dsObj, ...) { # nolint
  #TODO implement
})

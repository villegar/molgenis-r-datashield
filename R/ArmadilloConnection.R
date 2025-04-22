#' @include ArmadilloDriver.R

setOldClass("handle")

#' Class ArmadilloConnection.
#'
#' An Armadillo connection implementing the DataSHIELD Interface (DSI)
#' \code{\link[DSI]{DSConnection-class}}.
#'
#' @slot name The name of the connection
#' @slot handle The handle used to connect with the server
#' @slot user The username used to authenticate
#' @slot cookies The cookies set by the server
#' @slot token An optional authentication token (JWT)
#'
#' @importClassesFrom DSI DSConnection
#' @export
#' @keywords internal
methods::setClass("ArmadilloConnection",
  contains = "DSConnection",
  slots = list(
    name = "character",
    handle = "handle",
    user = "character",
    cookies = "list",
    token = "character"
  )
)

#' Disconnect from an Armadillo DataSHIELD Service
#'
#' Disconnect from an Armadillo DataSHIELD Service and release all R resources.
#' If a workspace ID is provided, the DataSHIELD R session will be saved before
#' being destroyed.
#'
#' @param conn \code{\link{ArmadilloConnection-class}} class
#' object
#' @param save Save the DataSHIELD R session with provided ID (must be a
#' character string).
#'
#' @importMethodsFrom DSI dsDisconnect
#' @seealso \code{\link[DSI]{dsDisconnect}}
#' @export
methods::setMethod(
  "dsDisconnect", "ArmadilloConnection",
  function(conn, save = NULL) {
    if (!is.null(save)) {
      response <- httr::POST(
        handle = conn@handle,
        path = paste0("/workspaces/", save),
        config = httr::add_headers(.get_auth_header(conn))
      )
      .handle_request_error(response)
    }
    httr::POST(
      handle = conn@handle,
      path = "/logout",
      config = httr::add_headers(.get_auth_header(conn))
    )
  }
)

methods::setMethod(
  "dsListProfiles", "ArmadilloConnection", function(conn) {

    tryCatch({
      new_conn <- .reset_token_if_expired(conn)
      if (!is.null(new_conn)) conn <- new_conn
    }, error = function(e) {
      paste0("Failed to reset token: ", e$message)
    })

    response <- httr::GET(
      handle = conn@handle,
      path = "/profiles",
      config = httr::add_headers(.get_auth_header(conn))
    )
    .handle_request_error(response)
    if (response$status_code == 404) {
      # endpoint not implemented, fake it!
      list(available = "default", current = "default")
    } else {
      httr::content(response)
    }
  }
)

#' List Armadillo DataSHIELD Service tables
#'
#' List Armadillo DataSHIELD Service tables that may be accessible for
#' performing DataSHIELD operations.
#'
#' @param conn \code{\link{ArmadilloConnection-class}} class object
#'
#' @return The fully qualified names of the tables.
#'
#' @seealso \code{\link[DSI]{dsListTables}}
#' @importMethodsFrom DSI dsListTables
#' @export
methods::setMethod(
  "dsListTables", "ArmadilloConnection", function(conn) {

    tryCatch({
      new_conn <- .reset_token_if_expired(conn)
      if (!is.null(new_conn)) conn <- new_conn
    }, error = function(e) {
      paste0("Failed to reset token: ", e$message)
    })

    response <- httr::GET(
      handle = conn@handle,
      path = "/tables",
      config = httr::add_headers(.get_auth_header(conn))
    )
    .handle_request_error(response)
    .unlist_character_list(httr::content(response))
  }
)

#' Verify table exist and can be accessible for performing DataSHIELD
#' operations.
#'
#' @param conn \code{\link{ArmadilloConnection-class}} class object.
#' @param table The identifier of the table
#'
#' @return TRUE if table exists.
#'
#' @importMethodsFrom DSI dsHasTable
#' @export
methods::setMethod(
  "dsHasTable", "ArmadilloConnection", function(conn, table) {

    tryCatch({
      new_conn <- .reset_token_if_expired(conn)
      if (!is.null(new_conn)) conn <- new_conn
    }, error = function(e) {
      paste0("Failed to reset token: ", e$message)
    })

    response <- httr::HEAD(
      handle = conn@handle,
      path = paste0("/tables/", table),
      config = httr::add_headers(.get_auth_header(conn))
    )

    .handle_request_error(response)

    response$status_code == 200
  }
)

#' List Armadillo DataSHIELD Service resources
#'
#' List Armadillo DataSHIELD Service resources that may be accessible for
#' performing DataSHIELD operations.
#'
#' @param conn \code{\link{ArmadilloConnection-class}} class object
#'
#' @return The fully qualified names of the resources.
#'
#' @seealso \code{\link[DSI]{dsListResources}}
#' @importMethodsFrom DSI dsListResources
#' @export
methods::setMethod(
  "dsListResources", "ArmadilloConnection", function(conn) {

    tryCatch({
      new_conn <- .reset_token_if_expired(conn)
      if (!is.null(new_conn)) conn <- new_conn
    }, error = function(e) {
      paste0("Failed to reset token: ", e$message)
    })

    response <- httr::GET(
      handle = conn@handle,
      path = "/resources",
      config = httr::add_headers(.get_auth_header(conn))
    )
    .handle_request_error(response)
    .unlist_character_list(httr::content(response))
  }
)

#' Verify resource exists.
#'
#' @param conn \code{\link{ArmadilloConnection-class}} class object.
#' @param resource The identifier of the resource
#'
#' @return TRUE if resource exists.
#'
#' @importMethodsFrom DSI dsHasResource
#' @export
methods::setMethod(
  "dsHasResource", "ArmadilloConnection", function(conn, resource) {

    tryCatch({
      new_conn <- .reset_token_if_expired(conn)
      if (!is.null(new_conn)) conn <- new_conn
    }, error = function(e) {
      paste0("Failed to reset token: ", e$message)
    })

    response <- httr::HEAD(
      handle = conn@handle,
      path = paste0("/resources/", resource),
      config = httr::add_headers(.get_auth_header(conn))
    )
    .handle_request_error(response)

    response$status_code == 200
  }
)

#' Armadillo DataShield Service asynchronous support
#'
#' List of DataSHIELD operations on which Armadillo DataSHIELD Service supports
#' asynchronicity.
#'
#' When a \code{\link[DSI]{DSResult-class}} object is returned on aggregation or
#' assignment operation, the raw result can be accessed asynchronously,
#' allowing parallelization of DataSHIELD calls over multpile servers.
#' The returned named list of logicals will specify if asynchronicity is
#' supported for:
#' aggregation operation ('aggregate'),
#' table assignment operation ('assignTable'),
#' resource assignment operation ('assignResource')
#' and expression assignment operation ('assignExpr').
#'
#' @param conn \code{\link{ArmadilloConnection-class}} class object
#'
#' @return The named list of logicals detailing the asynchronicity support.
#'
#' @importMethodsFrom DSI dsIsAsync
#' @export
methods::setMethod(
  "dsIsAsync", "ArmadilloConnection", function(conn) {
    list(
      aggregate = TRUE,
      assignTable = TRUE,
      assignResource = TRUE,
      assignExpr = TRUE
    )
  }
)

#' List R symbols
#'
#' List symbols living in the DataSHIELD R session.
#'
#' @param conn \code{\link{ArmadilloConnection-class}} class object
#'
#' @return A character vector.
#'
#' @importMethodsFrom DSI dsListSymbols
#' @export
methods::setMethod(
  "dsListSymbols", "ArmadilloConnection",
  function(conn) {

    tryCatch({
      new_conn <- .reset_token_if_expired(conn)
      if (!is.null(new_conn)) conn <- new_conn
    }, error = function(e) {
      paste0("Failed to reset token: ", e$message)
    })

    response <- httr::GET(
      handle = conn@handle,
      path = "/symbols",
      config = httr::add_headers(.get_auth_header(conn))
    )
    .handle_request_error(response)
    .unlist_character_list(httr::content(response))
  }
)

#' Remove an R symbol
#'
#' Remove a symbol living in the DataSHIELD R session. After removal, the data
#' identified by the symbol will not be accessible in the DataSHIELD R session
#' on the server side.
#'
#' @param conn \code{\link{ArmadilloConnection-class}} class object
#' @param symbol Name of the R symbol.
#'
#' @importMethodsFrom DSI dsRmSymbol
#' @export
methods::setMethod(
  "dsRmSymbol", "ArmadilloConnection",
  function(conn, symbol) {

    tryCatch({
      new_conn <- .reset_token_if_expired(conn)
      if (!is.null(new_conn)) conn <- new_conn
    }, error = function(e) {
      paste0("Failed to reset token: ", e$message)
    })

    response <- httr::DELETE(
      handle = conn@handle,
      path = paste0("/symbols/", symbol),
      config = httr::add_headers(.get_auth_header(conn))
    )
    .handle_request_error(response)
  }
)

#' Assign a table
#'
#' Assign a R-data.frame in the DataSHIELD R session.
#'
#' @param conn \code{\link{ArmadilloConnection-class}} object.
#' @param symbol Name of the R symbol.
#' @param table Fully qualified name of the table.
#' @param variables The variables to load.
#' @param missings Not supported
#' @param identifiers Not supported
#' @param id.name Not supported
#' @param async When set, do not wait for the result.
#'
#' @return A \code{\link{ArmadilloResult-class}} object.
#'
#' @importMethodsFrom DSI dsAssignTable
#' @export
methods::setMethod(
  "dsAssignTable", "ArmadilloConnection",
  function(conn, symbol, table, variables = NULL, missings = FALSE,
           identifiers = NULL, id.name = NULL, async = TRUE) { # nolint
    query <- list(table = table, symbol = symbol, async = async)
    if (!is.null(variables)) {
      query$variables <- paste(unlist(variables), collapse = ",")
    }

    tryCatch({
      new_conn <- .reset_token_if_expired(conn)
      if (!is.null(new_conn)) conn <- new_conn
    }, error = function(e) {
      paste0("Failed to reset token: ", e$message)
    })

    response <- httr::POST(
      handle = conn@handle,
      path = "/load-table",
      query = query,
      config = httr::add_headers(.get_auth_header(conn))
    )

    .handle_request_error(response)

    if (async) {
      result <- NULL
    } else {
      result <- .retry_until_last_result(conn)
    }
    methods::new("ArmadilloResult",
      conn = conn,
      rval = list(result = result, async = async)
    )
  }
)

#' Assign a resource
#'
#' Assign a resource in the DataSHIELD R session.
#'
#' @param conn An object that inherits from \code{\link[DSI]{DSConnection-class}}.
#' @param symbol Name of the R symbol.
#' @param resource Fully qualified name of a resource reference in the data
#' repository.
#' @param async Whether the result of the call should be retrieved
#' asynchronously.
#'
#' @return A \code{\link{ArmadilloResult-class}} object.
#'
#' @importMethodsFrom DSI dsAssignResource
#' @export
methods::setMethod(
  "dsAssignResource", "ArmadilloConnection",
  function(conn, symbol, resource, async = TRUE) {
    query <- list(resource = resource, symbol = symbol, async = async)

    tryCatch({
      new_conn <- .reset_token_if_expired(conn)
      if (!is.null(new_conn)) conn <- new_conn
    }, error = function(e) {
      paste0("Failed to reset token: ", e$message)
    })

    response <- httr::POST(
      handle = conn@handle,
      path = "/load-resource",
      query = query,
      config = httr::add_headers(.get_auth_header(conn))
    )

    .handle_request_error(response)

    if (async) {
      result <- NULL
    } else {
      result <- .retry_until_last_result(conn)
    }

    methods::new("ArmadilloResult",
      conn = conn,
      rval = list(result = result, async = async)
    )
  }
)


#' List methods
#'
#' List methods defined in the DataSHIELD configuration.
#'
#' @param conn \code{\link{ArmadilloConnection-class}} class object
#' @param type Type of the method: "aggregate" (default) or "assign".
#'
#' @return A data.frame with columns: name, type ('aggregate' or 'assign'),
#' class ('function' or 'script'), value, package, version.
#'
#' @importMethodsFrom DSI dsListMethods
#' @export
methods::setMethod(
  "dsListMethods", "ArmadilloConnection",
  function(conn, type = "aggregate") {

    tryCatch({
      new_conn <- .reset_token_if_expired(conn)
      if (!is.null(new_conn)) conn <- new_conn
    }, error = function(e) {
      paste0("Failed to reset token: ", e$message)
    })

    response <- httr::GET(
      handle = conn@handle,
      path = paste0("/methods/", type),
      config = httr::add_headers(.get_auth_header(conn))
    )
    .handle_request_error(response)
    df <- .list_to_data_frame(httr::content(response))
    df <- .fill_column(df, "type", type)
    df <- .fill_column(df, "class", "function")
    df <- .rename_column(df, "function", "value")
  }
)

#' List packages
#'
#' List packages with their versions defined in the DataSHIELD configuration.
#'
#' @param conn \code{\link{ArmadilloConnection-class}} class object
#'
#' @return A data.frame with columns: name, version.
#'
#' @importMethodsFrom DSI dsListPackages
#' @export
methods::setMethod(
  "dsListPackages", "ArmadilloConnection",
  function(conn) {

    tryCatch({
      new_conn <- .reset_token_if_expired(conn)
      if (!is.null(new_conn)) conn <- new_conn
    }, error = function(e) {
      paste0("Failed to reset token: ", e$message)
    })

    response <- httr::GET(
      handle = conn@handle,
      path = "/packages",
      config = httr::add_headers(.get_auth_header(conn))
    )
    .handle_request_error(response)

    extracted_cols <- lapply(
      httr::content(response),
      function(x) list(name = x$name, version = x$version)
    )
    .list_to_data_frame(extracted_cols)
  }
)

#' List workspaces
#'
#' List workspaces saved in the data repository.
#'
#' @param conn \code{\link{ArmadilloConnection-class}} class object
#'
#' @return A data.frame with columns: name, lastAccessDate, size, user.
#'
#' @importMethodsFrom DSI dsListWorkspaces
#' @export
methods::setMethod(
  "dsListWorkspaces", "ArmadilloConnection",
  function(conn) {

    tryCatch({
      new_conn <- .reset_token_if_expired(conn)
      if (!is.null(new_conn)) conn <- new_conn
    }, error = function(e) {
      paste0("Failed to reset token: ", e$message)
    })

    response <- httr::GET(
      handle = conn@handle,
      path = "/workspaces",
      config = httr::add_headers(.get_auth_header(conn))
    )
    .handle_request_error(response)

    df <- .list_to_data_frame(httr::content(response))
    df <- .fill_column(df, "user", conn@user)
    df <- .rename_column(df, "lastModified", "lastAccessDate")
    df
  }
)

#' Save workspace
#'
#' Save workspace on the data repository.
#'
#' @param conn \code{\link{ArmadilloConnection-class}} class object
#' @param name Name of the workspace.
#'
#' @importMethodsFrom DSI dsSaveWorkspace
#' @export
methods::setMethod(
  "dsSaveWorkspace", "ArmadilloConnection",
  function(conn, name) {

    tryCatch({
      new_conn <- .reset_token_if_expired(conn)
      if (!is.null(new_conn)) conn <- new_conn
    }, error = function(e) {
      paste0("Failed to reset token: ", e$message)
    })

    response <- httr::POST(
      handle = conn@handle,
      path = paste0("/workspaces/", name),
      config = httr::add_headers(.get_auth_header(conn))
    )
    .handle_request_error(response)
  }
)

#' Remove a workspace
#'
#' Remove a workspace on the data repository.
#'
#' @param conn \code{\link{ArmadilloConnection-class}} class object
#' @param name Name of the workspace.
#'
#' @importMethodsFrom DSI dsRmWorkspace
#' @export
methods::setMethod(
  "dsRmWorkspace", "ArmadilloConnection",
  function(conn, name) {

    tryCatch({
      new_conn <- .reset_token_if_expired(conn)
      if (!is.null(new_conn)) conn <- new_conn
    }, error = function(e) {
      paste0("Failed to reset token: ", e$message)
    })

    response <- httr::DELETE(
      handle = conn@handle,
      path = paste0("/workspaces/", name),
      config = httr::add_headers(.get_auth_header(conn))
    )
    .handle_request_error(response)
  }
)

#' Assign the result of an expression
#'
#' Assign a result of the execution of an expression in the DataSHIELD R
#' session.
#'
#' @param conn \code{\link{ArmadilloConnection-class}} object.
#' @param symbol Name of the R symbol.
#' @param expr A R expression with allowed assign functions calls.
#' @param async Whether the result of the call should be retrieved
#' asynchronously. When TRUE (default) the calls are parallelized over the
#' connections, when the connection supports that feature, with an extra
#' overhead of requests.
#'
#' @return A \code{\link{ArmadilloResult-class}} object.
#'
#' @importMethodsFrom DSI dsAssignExpr
#' @export
methods::setMethod(
  "dsAssignExpr", "ArmadilloConnection",
  function(conn, symbol, expr, async = TRUE) {

    tryCatch({
      new_conn <- .reset_token_if_expired(conn)
      if (!is.null(new_conn)) conn <- new_conn
    }, error = function(e) {
      paste0("Failed to reset token: ", e$message)
    })

    response <- httr::POST(
      handle = conn@handle,
      query = list(async = async),
      path = paste0("/symbols/", symbol),
      body = .deparse(expr),
      config = httr::add_headers(c("Content-Type" = "text/plain",
                                   .get_auth_header(conn)))
    )

    .handle_request_error(response)

    if (async) {
      result <- NULL
    } else {
      result <- .retry_until_last_result(conn)
    }

    methods::new("ArmadilloResult",
      conn = conn,
      rval = list(
        result = NULL,
        async = async
      )
    )
  }
)

#' Aggregate data
#'
#' Aggregate some data from the DataSHIELD R session using a valid R expression.
#' The aggregation expression must satisfy the data repository's DataSHIELD
#' configuration.
#'
#' @param conn \code{\link{ArmadilloConnection-class}} object.
#' @param expr Expression to evaluate.
#' @param async Whether the result of the call should be retrieved
#' asynchronously. When TRUE (default) the calls are parallelized over the
#' connections, when the connection supports that feature, with an extra
#' overhead of requests.
#'
#' @importMethodsFrom DSI dsAggregate
#' @export
methods::setMethod(
  "dsAggregate", "ArmadilloConnection",
  function(conn, expr, async = TRUE) {

    tryCatch({
      new_conn <- .reset_token_if_expired(conn)
      if (!is.null(new_conn)) conn <- new_conn
      conn
    }, error = function(e) {
      paste0("Failed to reset token: ", e$message)
    })

    response <- httr::POST(
      handle = conn@handle,
      query = list(async = async),
      path = "/execute",
      body = .deparse(expr),
      config = httr::add_headers(c("Content-Type" = "text/plain",
                                   .get_auth_header(conn)))
    )

    .handle_request_error(response)

    if (async) {
      result <- NULL
    } else {
      result <- .retry_until_last_result(conn)
    }

    methods::new("ArmadilloResult",
      conn = conn,
      rval = list(result = result, async = async)
    )
  }
)


#' Get connection info
#'
#' Get information about a connection.
#'
#' @param dsObj \code{\link{ArmadilloConnection-class}} class object
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
#' @importMethodsFrom DSI dsGetInfo
#' @export
methods::setMethod(
  "dsGetInfo", "ArmadilloConnection",
  function(dsObj, ...) { # nolint
    response <- httr::GET(
      handle = dsObj@handle,
      path = "/actuator/info",
      config = httr::add_headers(.get_auth_header(dsObj))
    )
    .handle_request_error(response)
    result <- httr::content(response)
    result$url <- dsObj@handle$url
    result$name <- dsObj@name
    result$cookies <- dsObj@cookies
    result
  }
)

#' Keep a connection alive
#'
#' As the DataSHIELD sessions are working in parallel, this function helps at
#' keeping idle connections alive while others are working. Any communication
#' failure must be silently processed.
#'
#' @param conn \code{\link{ArmadilloConnection-class}} class object
#'
#' @return NULL, invisibly
#'
#' @importMethodsFrom DSI dsKeepAlive
#' @export
methods::setMethod(
  "dsKeepAlive", "ArmadilloConnection",
  function(conn) { # nolint

    tryCatch({
      new_conn <- .reset_token_if_expired(conn)
      if (!is.null(new_conn)) conn <- new_conn
    }, error = function(e) {
      paste0("Failed to reset token: ", e$message)
    })

    try(httr::GET(
      handle = conn@handle,
      path = "/actuator/info",
      config = httr::add_headers(.get_auth_header(conn))
    ), silent = TRUE)
    invisible(NULL)
  }
)

#' @title Reset Armadillo Token if Expired
#' @description Checks if the current token has expired and refreshes it if necessary.
#' @param conn A `DSConnection` object.
#' @return The updated `DSConnection` object with a refreshed token if needed.
#' @keywords internal
#' @noRd
.reset_token_if_expired <- function(conn) {
  credentials <- .get_armadillo_credentials(conn)
  if(credentials$object@expires_at < Sys.time()) {
    new_credentials <- .refresh_token(conn@handle$url, credentials$object)
    conn@token <- new_credentials$token
    .reset_token_global_env(credentials, new_credentials, conn)
    return(conn)
  }
}

#' @title Get Armadillo Credentials
#' @description Retrieves the Armadillo credentials that match the provided connection.
#' @param conn A `DSConnection` object.
#' @param env The environment where credentials are stored. Defaults to `globalenv()` or `getOption("datashield.env")`.
#' @return A list with the name and object of the matched credentials, or `NULL` if no match is found.
#' @keywords internal
#' @noRd
.get_armadillo_credentials <- function(conn, env = getOption("datashield.env", globalenv())) {
  all_credentials <- .get_all_armadillo_credentials(env)
  if(is.null(all_credentials)) {
    return()
  } else {
  matching <- .get_matching_credential(all_credentials, conn)
  if(is.list(matching) && length(matching) == 0) {
    return()
  } else {
    return(matching)
    }
  }
}

#' @title Get All Armadillo Credentials
#' @description Scans the specified environment for objects of class `"ArmadilloCredentials"`.
#' @param env The environment to search. Defaults to `globalenv()` or `getOption("datashield.env")`.
#' @return A named list of `"ArmadilloCredentials"` objects.
#' @keywords internal
#' @noRd
.get_all_armadillo_credentials <- function(env = getOption("datashield.env", globalenv())) {
  objs <- ls(envir = env)
  conns <- sapply(objs, function(x) {
    obj <- get(x, envir = env)
    inherits(obj, "ArmadilloCredentials")
  }, USE.NAMES = TRUE)


  if(is.character(objs) && length(objs) == 0) {
    return()
  } else {
  matched <- objs[conns]
  return(mget(matched, envir = env))
  }
}

#' @title Get Matching Credential
#' @description Matches a token from a connection with stored credentials.
#' @param credentials A named list of `"ArmadilloCredentials"` objects.
#' @param conn A `DSConnection` object containing the token to match.
#' @return A list with the name and object of the matched credential, or `NULL` if not found.
#' @keywords internal
#' @noRd
.get_matching_credential <- function(credentials, conn) {
  target_token <- conn@token

  for (name in names(credentials)) {
    cred <- credentials[[name]]
    if (inherits(cred, "ArmadilloCredentials") && cred@access_token == target_token) {
      return(list(name = name, object = cred))
    }
  }

  return(NULL)
}


#' @title Reset Armadillo Credentials
#' @description Updates the stored credentials in the environment with refreshed tokens.
#' @param old_credentials A list with name and object of the old credentials.
#' @param new_credentials A list with new token and refreshToken values.
#' @param env The environment where credentials are stored. Defaults to `globalenv()` or `getOption("datashield.env")`.
#' @return None. Used for side-effects.
#' @keywords internal
#' @noRd
.reset_armadillo_credentials <- function(old_credentials, new_credentials, env = getOption("datashield.env", globalenv())) {
  credentials_to_return <- old_credentials$object
  credentials_to_return@access_token <- new_credentials$token
  credentials_to_return@refresh_token <- new_credentials$refreshToken
  assign(old_credentials$name, credentials_to_return, envir = env)
}

#' @title Reset Connections Object
#' @description Updates the token in the connections object to reflect refreshed credentials.
#' @param old_credentials A list with the old credentials.
#' @param new_credentials A list with new token values.
#' @param conn A `DSConnection` object.
#' @param env The environment containing the connections object. Defaults to `globalenv()` or `getOption("datashield.env")`.
#' @return None. Used for side-effects.
#' @keywords internal
#' @noRd
.reset_connections_object <- function(old_credentials, new_credentials, conn, env=getOption("datashield.env", globalenv())) {
  old_conns <- DSI:::.getDSConnections(env)
  old_conns$flag <- NULL
  conns_to_return <- old_conns$conns

  for (i in seq_along(conns_to_return)) {
    if (methods::slot(conns_to_return[[i]], "token") == old_credentials$object@access_token) {
      methods::slot(conns_to_return[[i]], "token") <- new_credentials$token
      break  # stop after first match
    }
  }
  assign(names(old_conns), conns_to_return, envir = env)
}

#' @title Reset Token in Global Environment
#' @description Updates both the Armadillo credentials and the connection object in the global environment.
#' @param old_credentials A list with the old credentials.
#' @param new_credentials A list with new token and refreshToken values.
#' @param conn A `DSConnection` object.
#' @param env The environment where updates should be made. Defaults to `globalenv()` or `getOption("datashield.env")`.
#' @return None. Used for side-effects.
#' @keywords internal
#' @noRd
.reset_token_global_env <- function(old_credentials, new_credentials, conn, env=getOption("datashield.env", globalenv())) {
  .reset_armadillo_credentials(old_credentials, new_credentials, env)
  .reset_connections_object(old_credentials, new_credentials, conn, env)
}

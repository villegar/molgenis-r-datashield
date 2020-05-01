#' @include MolgenisDriver.R MolgenisConnection.R
NULL

#' Class MolgenisResult.
#'
#' A MOLGENIS result implementing the DataSHIELD Interface (DSI)
#' \code{\link{DSResult-class}}.
#'
#' @slot conn The connection used to create this result
#' @slot rval The result
#'
#' @importClassesFrom DSI DSResult
#' @export
#' @keywords internal
methods::setClass("MolgenisResult",
  contains = "DSResult",
  slots = list(
    conn = "MolgenisConnection",
    rval = "list"
  )
)

#' Get result info
#'
#' Get the information about a command (if still available).
#'
#' @param dsObj \code{\link{MolgenisResult-class}} class object
#' @param ... Unused, needed for compatibility with generic.
#'
#' @return The result information. This should include the R expression
#' being executed (`expression`) and if the query is complete
#' (`{ "status" = "COMPLETED" }`).
#'
#' @importMethodsFrom DSI dsGetInfo
#' @export
methods::setMethod(
  "dsGetInfo", "MolgenisResult",
  function(dsObj, ...) { # nolint
    if (dsObj@rval$async) {
      result <- httr::GET(
        handle = dsObj@conn@handle,
        url = dsObj@conn@handle$url,
        path = "/lastcommand",
        httr::add_headers("Accept" = "application/json")
      )
      httr::content(result)
    } else {
      list(status = "COMPLETED")
    }
  }
)

#' Fetch the result
#'
#' Fetch the DataSHIELD operation result.
#'
#' @param res \code{\link{MolgenisResult-class}} object.
#'
#' @return TRUE if table exists.
#'
#' @importMethodsFrom DSI dsFetch
#' @export
methods::setMethod(
  "dsFetch", "MolgenisResult",
  function(res) {
    if (res@rval$async) {
      .retry_until_last_result(res@conn)
    } else {
      res@rval$result
    }
  }
)

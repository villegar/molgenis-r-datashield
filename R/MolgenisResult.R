#' @include MolgenisDriver.R MolgenisConnection.R
NULL

#' Class MolgenisResult.
#'
#' A MOLGENIS result implementing the DataSHIELD Interface (DSI)  \code{\link{DSResult-class}}.
#' 
#' @import methods
#' @import DSI
#' @export
#' @keywords internal
setClass("MolgenisResult", contains = "DSResult", slots = list(
  conn = "MolgenisConnection",
  rval = "list"))

#' Get result info
#' 
#' Get the information about a command (if still available).
#' 
#' @param dsObj \code{\link{MolgenisResult-class}} class object
#' @param ... Unused, needed for compatibility with generic.
#' 
#' @return The result information. This should include the R expression
#' being executed (`expression`) and if the query is complete (`has.completed`).
#' 
#' @import methods
#' @export
setMethod("dsGetInfo", "MolgenisResult", function(dsObj, ...) {
  list(status="COMPLETED")
})

#' Fetch the result
#' 
#' Fetch the DataSHIELD operation result.
#' 
#' @param res \code{\link{MolgenisResult-class}} object.
#' 
#' @return TRUE if table exists.
#' 
#' @import methods
#' @export
setMethod("dsFetch", "MolgenisResult", function(res) {
  if (res@rval$async) {
    rawResult <- RETRY(verb="GET",
                     handle=res@conn@props$handle,
                     url=res@conn@props$handle.url,
                     path="/lastresult",
                     times=5,
                     add_headers('Accept'='application/octet-stream'))
    print('hello')
    unserialize(content(rawResult))
  } else {
    res@rval$result
  }
})
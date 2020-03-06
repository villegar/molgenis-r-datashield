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
#' @return The result information, including its status.
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
  res@rval$result
})
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
#' being executed (`expression`) and if the query is complete (`{ "status" = "COMPLETED" }`).
#' 
#' @import methods
#' @export
setMethod("dsGetInfo", "MolgenisResult", function(dsObj, ...) {
  if (dsObj@rval$async) {
    result <- GET(handle=dsObj@conn@handle,
                  url=dsObj@conn@handle$url,
                  path="/lastcommand",
                  add_headers('Accept'='application/json'))
    content(result)
  } else {
    list(status="COMPLETED") 
  }
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
    command <- GET(handle=res@conn@handle, path="/lastcommand")
    
    if (content(command)$withResult == TRUE){
      response <- RETRY(verb="GET",
                         handle=res@conn@handle,
                         url=res@conn@handle$url,
                         path="/lastresult",
                         times=5,
                         add_headers('Accept'='application/octet-stream'))
    
      .handleLastCommandError(res@conn@handle)
        
      unserialize(content(response)) 
    }else{
      # TODO wait for /lastresult to be finished (will have empty body) and then check /lastcommand for errors
      NULL
    }
  } else {
    res@rval$result
  }
})
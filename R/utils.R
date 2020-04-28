#' @keywords internal
.handleLastCommandError <- function(handle) {
  command <- GET(handle=handle, path="/lastcommand")
  
  jsonContent = content(command)
  if (jsonContent$status == "FAILED"){
    stop(paste0("Execution failed: ", jsonContent$message))
  }
}

#' @keywords internal
.handleRequestError <- function(response) {
  if (response$status_code == 400){
    jsonContent = content(response)
    stop(paste0("Bad request: ", jsonContent$message))
  }else if(response$status_code == 401){
    stop("Unauthorized")
  }
}

#' @keywords internal
.unlistCharacterList <- function(characterList) {
  if (length(characterList) == 0){
    character()
  }else{
    unlist(characterList)
  }
}

#' @keywords internal
.listToDataFrame <- function(list) {
  as.data.frame(do.call(rbind, list))
}

#' @keywords internal
.retryUntilLastResult <- function(conn) {
  response <- RETRY(verb="GET",
                    handle=conn@handle,
                    url=conn@handle$url,
                    path="/lastresult",
                    terminate_on = c(200, 404, 401),
                    add_headers('Accept'='application/octet-stream'))
  
  .handleRequestError(response)
  
  if (response$status_code == 404){
    .handleLastCommandError(conn@handle)  
  }else{
    content <- content(response)
    if (is.null(content)){
      NULL
    }else{
      unserialize(content)
    }
  }
}

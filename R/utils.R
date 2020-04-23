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
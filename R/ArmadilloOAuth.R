#' Get ID Token
#'
#' Get an ID token to log in on an Armadillo server or its MinIO filestore.
#'
#' @param server the URL of the Armadillo server
#'
#' @return The ID token string
#'
#' @importFrom MolgenisAuth discover device_flow_auth
#'
#' @export
datashield.get_token <- function(server) { #nolint
  auth_info <- .get_oauth_info(server)$auth
  endpoint <- MolgenisAuth::discover(auth_info$issuerUri)
  credentials <- MolgenisAuth::device_flow_auth(
    endpoint,
    auth_info$clientId
  )
  return(credentials$id_token)
}

#' Get oauth server discovery information
#'
#' Specifically this method returns the
#' - issuer URL
#' - clientId
#'
#' @param armadillo_server url of the Armadillo server
#'
#' @importFrom httr GET stop_for_status content
#' @importFrom urltools path
#'
#' @return a dataframe with issuerUrl and clientId
#'
#' @noRd
.get_oauth_info <- function(armadillo_server) {
  info_url <- armadillo_server
  urltools::path(info_url) <- "actuator/info"
  response <- httr::GET(info_url)
  httr::stop_for_status(response, task = "fetch server info")
  return(httr::content(response))
}

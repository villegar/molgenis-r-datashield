#' Get ID Token
#'
#' Get an ID token to log in on an Armadillo server
#'
#' @param server the URL of the Armadillo server
#'
#' @return The ID token string
#'
#' @importFrom MolgenisAuth discover device_flow_auth
#'
#' @export
armadillo.get_token <- function(server) { # nolint
  credentials <- armadillo.get_credentials(server)
  return(credentials@id_token)
}

#' @title ArmadilloCredentials Class
#' @description An S4 class to represent authentication credentials used for accessing Armadillo servers.
#' @slot access_token Character. The access token used for authentication.
#' @slot expires_in Numeric. The number of seconds until the token expires.
#' @slot expires_at POSIXct. The timestamp when the token expires.
#' @slot id_token Character. The ID token containing identity information.
#' @slot refresh_token Character. The token used to obtain a new access token when expired.
#' @slot token_type Character. The type of token (typically "Bearer").
#' @slot userId Character. The unique identifier of the user.
#' @keywords internal
#' @export
setClass(
  "ArmadilloCredentials",
  slots = list(
    access_token = "character",
    expires_in = "numeric",
    expires_at = "POSIXct",
    id_token = "character",
    refresh_token = "character",
    token_type = "character",
    userId = "character")
)


#' Get credentials information
#'
#' Get credentials, including refresh, id and access token via device flow auth
#' NOTE: in order to get refresh token, refresh tokens should be turned on for this armadillo instance on the auth server
#'
#' @param server the URL of the Armadillo server
#'
#' @return The credentials
#'
#' @importFrom MolgenisAuth discover device_flow_auth
#'
#' @export
armadillo.get_credentials <- function(server) { # nolint
  auth_info <- .get_oauth_info(server)$auth
  endpoint <- MolgenisAuth::discover(auth_info$issuerUri)
  credentials <- MolgenisAuth::device_flow_auth(
    endpoint,
    auth_info$clientId
  )
  credentials_obj <- new("ArmadilloCredentials",  access_token = credentials$access_token,
                         expires_in =  credentials$expires_in,
                         expires_at = Sys.time() + credentials$expires_in,
                         id_token =  credentials$id_token,
                         refresh_token =  credentials$refresh_token,
                         token_type =  credentials$token_type,
                         userId =  credentials$userId)

  return(credentials_obj)
}

.refresh_token <- function(server, credentials) {
  message("\nAttempting refresh...")
  # get auth url
  auth_info <- .get_oauth_info(server)
  # post to fusionauth refresh endpoint with current access/refresh tokens
  fusionAuthRefreshUri <- paste0(auth_info$auth$issuerUri, "/api/jwt/refresh")
  response <- httr::POST(fusionAuthRefreshUri, handle=handle(''),
                         config=httr::set_cookies(refresh_token=credentials@refresh_token, access_token=credentials@access_token))
  new_credentials <- content(response)
  if (!is.null(new_credentials$refreshToken)) {
    message("Refresh successful")
  } else {
    if (!is.null(new_credentials$fieldErrors)){
      stop(paste0(" ", unlist(new_credentials$fieldErrors)))
    } else {
      stop("Refresh failed")
    }
  }
  return(new_credentials)
}

#' Refresh connections of all armadillo connections
#'
#' Refreshes a timed out connections using credentials containing the old access token and newest refresh token
#' Old credentials will be overwritten in the connections object to be able to continue analysis
#' NOTE: make sure refresh is possible for the armadillo instance on the auth server
#'
#' @param server the URL of the Armadillo server
#' @param credentials a list containing "refresh_token" and "access_token", can be retrieved using armadillo.get_credentials
#' @param conns the armadillo connections object
#' @param connections_name the name of the connections object in your environment
#' @param env your environment (default: .GlobalEnv)
#'
#' @return A list of credentials containing a new refresh_token and access_token "token" that can be used to retrieved data again
#'
#' @importFrom httr POST set_cookies
#'
#' @export
armadillo.refresh_connections <- function(server, credentials, conns, connections_name, env=.GlobalEnv) {
  new_credentials <- .refresh_token(server, credentials)
  for (i in 1:length(conns)) {
    conns[[i]]@token <- new_credentials$token
  }
  assign(connections_name, conns, envir=env)
  return(new_credentials)
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

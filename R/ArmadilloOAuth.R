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
#' @return The credentials
#' @importFrom MolgenisAuth discover device_flow_auth
#' @importFrom methods new
#' @export
armadillo.get_credentials <- function(server) { # nolint
  auth_info <- .get_oauth_info(server)$auth
  endpoint <- MolgenisAuth::discover(auth_info$issuerUri)
  credentials <- device_flow_auth(
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

#' Refresh OAuth token using FusionAuth
#'
#' Attempts to refresh the OAuth access token using the FusionAuth refresh endpoint.
#' It uses the current `access_token` and `refresh_token` from the provided credentials,
#' and performs a POST request to the FusionAuth server.
#'
#' @param server Character. The URL of the Armadillo server.
#' @param credentials An `ArmadilloCredentials` S4 object containing the current tokens.
#' @return A list containing the new credentials returned by the refresh endpoint.
#' @importFrom httr POST set_cookies content handle
#' @noRd
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

#' @title Reset Armadillo Token if Expired with tryCatch
#' @description Tries to refresh token and not break things if this fails
#' @param conn A `DSConnection` object.
#' @param env The DataSHIELD connection environment
#' @return The updated `DSConnection` object with a refreshed token if needed.
#' @keywords internal
#' @noRd
.refresh_token_safely <- function(conn, env = getOption("datashield.env", globalenv())) {
  tryCatch({
    conn <- .reset_token_if_expired(conn, env)
    if (inherits(conn, "ArmadilloConnection")) conn
  }, error = function(e) {
    warning("Failed to reset token: ", e$message)
    NULL
  })
}

#' @title Reset Armadillo Token if Expired
#' @description Checks if the current token has expired and refreshes it if necessary.
#' @param conn A `DSConnection` object.
#' @return The updated `DSConnection` object with a refreshed token if needed.
#' @keywords internal
#' @noRd
.reset_token_if_expired <- function(conn, env = getOption("datashield.env", globalenv())) {
  credentials <- .get_armadillo_credentials(conn)
  if(credentials$object@expires_at < Sys.time()) {
    multiple_conns <- identical(names(.getDSConnectionsMod(env)), c("flag", "conns")) &!is.null(names(.getDSConnectionsMod(env)$conns))
    if(multiple_conns) {
      return(
        warning("Token has expired however it was not possible to refresh token because multiple DataSHIELD connection objects found in environment. Please ensure only one exists and try again"))
    }
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
#' @importFrom DSI datashield.connections_find
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
  old_conns <- .getDSConnectionsMod(env)
  old_conns$flag <- NULL
  conns_to_return <- old_conns[[1]]

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
  .reset_connections_object(old_credentials, new_credentials, conn, sys.frame(1))
}

#' Detect DataSHIELD Connections in an Environment
#'
#' Scans a given environment for DataSHIELD connection objects. Slightly modified from original version in DSI
#'
#' @param env An environment object to search for DataSHIELD connections.
#'
#' @return A list with:
#' \describe{
#'   \item{flag}{Indicator of result type:
#'     \itemize{
#'       \item 0: No connections found.
#'       \item 1: One connection list found.
#'       \item 2: Multiple connection lists found.
#'     }}
#'   \item{conns}{Either the connection list (if one found) or a character vector with the names of multiple connection lists.}
#' }
#'
#' @keywords internal
#' @noRd
.getDSConnectionsMod <- function(env) {
  symbols <- base::ls(name=env)
  if (length(symbols) > 0) {
    connlist <- c()
    flag <- 0
    for (i in 1:length(symbols)) {
      obj <- base::get(symbols[i], envir = env)
      if ("list" %in% class(obj)) {
        if (length(obj) > 0) {
          if (.isDSConnection(obj[[1]])) {
            connlist <- append(connlist, symbols[i])
            flag <- 1
          }
        }
      }
    }
    if (flag == 1) {
      if (length(connlist) > 1) {
        flag <- 2
        return(list(flag=flag, conns=connlist))
      } else {
        pp <- connlist[[1]]
        conns <- base::get(pp, envir = env)
        conns_to_return <- list(flag=flag, conns=conns)
        names(conns_to_return) <- c("flag", pp)
        return(conns_to_return)
      }
    }
  }
  return(list(flag=0, conns=NULL))
}

#' Check if provided object is a S4 class instance and if this class inherits from \code{\link{DSConnection-class}}.
#' @keywords internal
#' @importFrom methods getClass
#' @noRd
.isDSConnection <- function(obj) {
  if (isS4(obj)) {
    cls <- getClass(class(obj)[[1]])
    "DSConnection" %in% names(cls@contains)
  } else {
    FALSE
  }
}

#' Handle last command errors
#'
#' When the last command endpoint fails, handle errors
#'
#' @param handle HTTR handle
#'
#' @importFrom httr GET content
#'
#' @return error message only
#'
#' @noRd
.handle_last_command_error <- function(handle) {
  command <- httr::GET(handle = handle, path = "/lastcommand")

  json_content <- httr::content(command)
  if (json_content$status == "FAILED") {
    stop(paste0("Execution failed: ", json_content$message), call. = FALSE)
  }
}

#' Handle generic request errrors
#'
#' @param response HTTR response
#'
#' @importFrom httr content
#'
#' @noRd
.handle_request_error <- function(response) {
  if (response$status_code == 400) {
    json_content <- httr::content(response)
    stop(paste0("Bad request: ", json_content$message), call. = FALSE)
  } else if (response$status_code == 401) {
    stop("Unauthorized", call. = FALSE)
  } else if (response$status_code == 500) {
    json_content <- httr::content(response)
    stop(paste0("Internal server error: ", json_content$message), call. = FALSE)
  }
}

#' Unlist character list
#'
#' @param character_list
#'
#' @noRd
.unlist_character_list <- function(character_list) {
  if (length(character_list) == 0) {
    character()
  } else {
    unlist(character_list)
  }
}

#' Convert list to data.frame
#'
#' @param list list object in R
#'
#' @noRd
.list_to_data_frame <- function(list) {
  as.data.frame(do.call(rbind, list))
}

#' Retry request after timeout
#'
#' @param conn HTTR connection
#'
#' @importFrom httr content add_headers
#'
#' @noRd
.retry_until_last_result <- function(conn) {
  response <- httr::RETRY(
    verb = "GET",
    handle = conn@handle,
    path = "/lastresult",
    terminate_on = c(200, 404, 401),
    config = httr::add_headers(c("Accept" = "application/octet-stream", .get_auth_header(conn)))
  )

  .handle_request_error(response)

  if (response$status_code == 404) {
    .handle_last_command_error(conn@handle)
  } else {
    content <- httr::content(response)
    if (is.null(content)) {
      NULL
    } else {
      unserialize(content)
    }
  }
}

#' Rename a column
#'
#' @param data_frame data.frame containing the column
#' @param name old name
#' @param new_name new name
#'
#' @noRd
.rename_column <- function(data_frame, name, new_name) {
  colnames(data_frame)[colnames(data_frame) == name] <- new_name
  data_frame
}

#' Fill column with a value
#'
#' @param data_frame containing the column
#' @param column designated column
#' @param value new value
#'
#' @noRd
.fill_column <- function(data_frame, column, value) {
  if (length(data_frame) > 0) {
    data_frame[column] <- value
  }
  data_frame
}

#' Deparse expression
#'
#' This function turns unevaluated expressions
#' (where ‘expression’ is taken in a wider sense than the
#' strict concept of a vector of mode "expression" used in expression)
#' into character strings (a kind of inverse to parse).
#'
#' @param expr expression to parse
#'
#' @seealso \code{\link{deparse}}
#'
#' @noRd
.deparse <- function(expr) {
  expression <- expr
  # convert a call to a string
  if (is.language(expr)) {
    expression <- deparse(expr)
    if (length(expression) > 1) {
      expression <- paste(expression, collapse = "\n")
    }
  } else if (!is.character(expr)) {
    stop("Invalid expression: '", class(expr), "'. Expected a call or vector.")
  }
  return(expression)
}

#' Creates an authorization header if the connection was created using a token.
#' 
#' @param conn HTTR connection
#' 
#' @noRd
.get_auth_header <- function(conn) {
  if (stringr::str_length(conn@token) > 0) {
    c("Authorization" = paste0("Bearer ", conn@token))
  } else {
    c()
  }
}
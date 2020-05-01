#' @keywords internal
.handle_last_command_error <- function(handle) {
  command <- httr::GET(handle = handle, path = "/lastcommand")

  json_content <- httr::content(command)
  if (json_content$status == "FAILED") {
    stop(paste0("Execution failed: ", json_content$message), call. = FALSE)
  }
}

#' @keywords internal
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

#' @keywords internal
.unlist_character_list <- function(character_list) {
  if (length(character_list) == 0) {
    character()
  } else {
    unlist(character_list)
  }
}

#' @keywords internal
.list_to_data_frame <- function(list) {
  as.data.frame(do.call(rbind, list))
}

#' @keywords internal
.retry_until_last_result <- function(conn) {
  response <- httr::RETRY(
    verb = "GET",
    handle = conn@handle,
    url = conn@handle$url,
    path = "/lastresult",
    terminate_on = c(200, 404, 401),
    httr::add_headers("Accept" = "application/octet-stream")
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

#' @keywords internal
.rename_column <- function(data_frame, name, new_name) {
  colnames(data_frame)[colnames(data_frame) == name] <- new_name
}

#' @keywords internal
.fill_column <- function(data_frame, column, value) {
  if (length(data_frame) > 0) {
    data_frame[column] <- value
  }
}

#' @keywords internal
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

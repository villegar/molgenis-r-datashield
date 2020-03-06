
#' Class MolgenisDriver with constructor Molgenis
#'
#' An Molgenis DataSHIELD Service Driver implementing the DataSHIELD Interface (DSI) \code{\link{DSDriver-class}}.
#' This class should always be initialized with the \code{\link{Molgenis}} function.
#' It returns a singleton that allows you to connect to Molgenis.
#' 
#' @import methods
#' @import DSI
#' @export
#' @keywords internal
setClass("MolgenisDriver", contains = "DSDriver")

#' Create a MOLGENIS DataSHIELD Service driver
#' 
#' Convenient function for creating a [MolgenisDriver] object.
#' 
#' @import methods
#' @import DSI
#' @export
Molgenis <- function() {
  new("MolgenisDriver")
}

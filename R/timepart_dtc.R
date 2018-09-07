#' Extract Time From Date/Time Objects
#'
#' This function takes a date-time object and its format as inputs, and returns the time portion.
#'
#' @usage timepart_dtc(x, format, outfmt, tz)
#'
#' @param x A date-time object, as a character string, formatted according to \code{format}.
#' @param format The format of \code{x}; default is to assume that a \code{T} separates the date from the time, as with CDISC data.
#' @param outfmt The format (as character string) to use for the result.
#' @param tz The desired \link{timezone}; default is "\code{UTC}".
#'
#' @return A character string with the time portion of \code{x}.
#'
#' @author Ryan Kyle, \email{ryan.kyle@mail.mcgill.ca}
#' @keywords utilities
#'
#' @export
#'
timepart_dtc <- function(x, format="%Y-%m-%dT%H:%M:%S", outfmt="%H:%M:%S", tz="UTC") {
  timeobj <- strptime(x, format, tz=tz)
  strftime(timeobj, format = outfmt)
}

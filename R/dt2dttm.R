#' Create Date/Time Object
#'
#' This function takes a time and a date, and melds them together.
#'
#' @param dateobj A date formatted as a character string.
#' @param timeobj A time formatted as a character string.
#' @param format A character string containing the format of the resulting date-time object.
#' @param tz The desired \link{timezone} to specify for the result.
#'
#' @return A object of class \code{POSIXct}, formatted according to \code{format} and \code{tz}.
#'
#' @examples
#' dt2dttm("2018-04-04", "15:15:00") # returns "2018-04-04 15:15:00 UTC"
#'
#' @author Ryan Kyle, \email{ryan.kyle@mail.mcgill.ca}
#' @keywords utilities
#'
#' @export
#'
dt2dttm <- function(dateobj, timeobj, format="%Y-%m-%d %H:%M:%S", tz = "UTC") {
  as.POSIXct(paste0(dateobj, timeobj, format), tz)
}

#' Add or Subtract Time from a Date-Time Object
#'
#' Computes difftime for date-time objects in R, removing separators from CDISC-typical character strings as needed.
#'
#' @usage adjust_dtc(time1, timediff, units, tz, POSIX)
#'
#' @param time1 Date-time object in character format.
#' @param timediff Value by which to increment or decrement.
#' @param units Units of \code{timediff}; i.e. minutes, hours, days or weeks.
#' @param tz POSIX-compliant time zone.
#' @param POSIX Boolean; if \code{TRUE}, output POSIXct object, otherwise output character string.
#'
#' @return Either character string or POSIXct object, depending on \code{POSIX} parameter.
#'
#' @author Ryan Kyle, \email{ryan.kyle@mail.mcgill.ca}
#' @keywords utilities
#'
#' @export
#'
adjust_dtc <- function(time1, timediff, units="seconds", tz="UTC", POSIX=FALSE) {
  chardate1 <- gsub(unname(unlist(time1)), pattern = "T", replacement = " ")

  posixdt1 <- as.POSIXct(chardate1, tz=tz)

  if(units=="minutes") {
    timediff <- timediff*(60*1)
  } else if(units=="hours") {
    timediff <- timediff*(60*60)
  } else if(units=="days") {
    timediff <- timediff*(60*60*24)
  } else if(units=="weeks") {
    timediff <- timediff*(60*60*24*7)
  }

  result <- posixdt1 + timediff

  if(POSIX) result <- as.POSIXct(result)
  else result <- gsub(" ", "T", result)

  return(result)
}

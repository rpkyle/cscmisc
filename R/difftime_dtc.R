#' Time Differences for Date/Time Objects
#'
#' This function takes two date-time objects, with or without date separator,
#' and estimates the time difference between them.
#'
#' The function allows for date-time objects of class character or POSIXct/POSIXlt;
#' it will convert either or both inputs to POSIX-compliant objects as required.
#'
#' The result is returned as a numerical difference (real number), or as a difftime
#' object.
#'
#' @usage difftime_dtc(date1, date2, units, tz, as.real)
#'
#' @param date1 A date, a character string in date-time format.
#' @param date2 A second date, a character string in date-time format.
#' @param units Units for calculation, defaults to \code{hours}.
#' @param tz Time-zone, defaults to "\code{UTC}".
#' @param as.real Boolean; should result be returned as a real number?
#'
#' @return Either a numeric value corresponding to the time difference between the inputs, or a \code{difftime} object.
#'
#' @author Ryan Kyle, \email{ryan.kyle@mail.mcgill.ca}
#' @keywords utilities
#'
#' @export
#'
difftime_dtc <- function(date1, date2, units="hours", tz="UTC", as.real=FALSE) {
  if(!("POSIXlt" %in% class(date1) | "POSIXct" %in% class(date1))) {
    chardate1 <- gsub(unname(unlist(date1)), pattern = "T", replacement = " ")
    posixdt1 <- as.POSIXct(chardate1, tz=tz)
  } else posixdt1 <- date1

  if(!("POSIXlt" %in% class(date2) | "POSIXct" %in% class(date2))) {
    chardate2 <- gsub(unname(unlist(date2)), pattern = "T", replacement = " ")
    posixdt2 <- as.POSIXct(chardate2, tz=tz)
  } else posixdt2 <- date2

  result <- difftime(posixdt1, posixdt2, units=units, tz=tz)

  if(as.real) return(result)
  else return(as.numeric(result))
}

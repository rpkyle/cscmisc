#' Determine Whether Numeric Value Lies Within Specified Interval
#'
#' This is a simple function to check whether a value is contained within the interval \code{[lo, hi]}. The
#' function also allows specifying whether the interval is closed on the left, i.e. \code{(lo, hi])}, on the right only,
#' i.e. \code{[lo, hi)}, or both \code{(lo, hi)}.
#'
#' @usage in_interval(x, lo, hi, closed.right=TRUE, closed.left=TRUE)
#'
#' @param x A numeric value.
#' @param lo A numeric value specifying the lower bound of the interval.
#' @param hi A numeric value specifying the upper bound of the interval.
#' @param closed.right Boolean; is the interval closed on the right (upper) side? Defaults to \code{TRUE}.
#' @param closed.left Boolean; is the interval closed on the left (lower) side? Defaults to \code{TRUE}.
#'
#' @return \code{TRUE} if the value \code{x} lies within the interval, \code{FALSE} if it does not.
#'
#' @author Ryan Kyle, \email{ryan.kyle@mail.mcgill.ca}
#' @keywords utilities
#'
#' @export
#'
in_interval <- function (x, lo, hi, closed.right=TRUE, closed.left=TRUE) {
  if(closed.right & closed.left) (x >= lo & x <= hi)
  else if(closed.right==FALSE & closed.left==FALSE) {
    (x > lo & x < hi)
  }
  else if(closed.right==FALSE) {
    (x >= lo & x < hi)
  }
  else if(closed.left==FALSE) {
    (x > lo & x <= hi)
  }
}

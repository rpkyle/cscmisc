#' Convert Factor to Numeric
#'
#' This function will convert a factor to a vector of numeric values. This is a simple convenience function.
#'
#' @usage f2n(x)
#'
#' @param x A factor containing numeric information to convert to a vector of numeric values (which is no longer a factor).
#'
#' @author Ryan Kyle, \email{ryan.kyle@mail.mcgill.ca}
#'
#' @keywords utilities
#'
#' @export
#'
f2n <- function(x) {
  as.numeric(levels(x))[x]
}

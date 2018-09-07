#' Interleave Two Vectors
#'
#' This function takes two vectors and merges them, by means of an in shuffle. Every other value in the result vector originates
#' from the same source vector; values in odd-numbered positions are from vector A, while even-numbered positions are occupied
#' by values from vector B. The ordering of the original values is preserved.
#'
#' @usage interleave(a, b)
#'
#' @param a A vector \code{A}.
#' @param b A second vector \code{B}.
#'
#' @return An interleaved vector with length equal to \code{length(A) + length(B)}.
#'
#' @examples
#' a <- c(1, 2, 3)
#' b <- c(4, 5, 6)
#'
#' interleave(a, b) # returns 1 4 2 5 3 6
#' interleave(as.character(a), as.character(b)) # returns "1" "4" "2" "5" "3" "6"
#'
#' @references \url{https://stackoverflow.com/questions/16443260/interleave-lists-in-r}
#'
#' @author Arun Srinivasan
#' @keywords utilities
#'
#' @export
#'
interleave <- function (a,b) {
  c(a,b)[order(c(seq_along(a), seq_along(b)))]
}

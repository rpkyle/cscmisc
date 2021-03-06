#' Vectorized Sequence Generation
#'
#' Generate regular sequences. \code{seq} is a standard generic with a default method. This is a vectorized version of the \code{seq}
#' function from base R.
#'
#' @usage seq(from = 1, to = 1, by = ((to - from)/(length.out - 1)), length.out = NULL, along.with = NULL, ...)
#'
#' @param ... arguments passed to or from methods.
#' @param from the starting value of the sequence. Of length 1 unless just from is supplied as an unnamed argument.
#' @param to the ending (maximal) value of the sequence. Of length 1 unless just from is supplied as an unnamed argument.
#' @param by number: increment of the sequence.
#' @param length.out desired length of the sequence. A non-negative number, which for seq and seq.int will be rounded up if fractional.
#' @param along.with take the length from the length of this argument.
#'
#' @return A matrix of vectors, with each column corresponding to a sequence specified by \code{from} and \code{to}.
#'
#' @examples
#' vecseq(from = c(1, 1.75), to = c(2, 2.75), by = 0.25)
#'
#' # results in:
#' #      [,1] [,2]
#' # [1,] 1.00 1.75
#' # [2,] 1.25 2.00
#' # [3,] 1.50 2.25
#' # [4,] 1.75 2.50
#' # [5,] 2.00 2.75
#'
#' @author Magically generated by the \code{\link{Vectorize}} function.
#' @keywords utilities
#'
#' @export
#'
vecseq <- Vectorize(seq.default, vectorize.args = c("from", "to"))

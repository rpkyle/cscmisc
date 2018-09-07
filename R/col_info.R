#' Produce an Object Detail Table
#'
#' This function will print object details to the console for each column in a \code{data.frame}, \code{data.table},
#' or an individual vector.
#'
#' Currently, the function will return the \code{\link{mode}}, \code{\link{storage.mode}}, and \code{\link{class}} of
#' an object. It will also specify whether or not the object is a \link{factor}.
#'
#' @usage col_info(x)
#'
#' @param x A \code{\link{data.frame}}, \code{\link{data.table}}, or vector.
#'
#' @details While the \code{\link{storage.mode}} and \code{\link{class}} of an object are often identical, they may not be,
#' e.g. when a vector has been encoded as a factor.
#'
#' @return \code{data.frame} containing the \code{\link{mode}}, \code{\link{class}}, \code{\link{storage.mode}}, and \code{\link{factor}} status.
#'
#' @author Ryan Kyle, \email{ryan.kyle@mail.mcgill.ca}
#' @keywords utilities
#'
#' @export
#'
col_info <- function(x) {
  if(is.data.frame(x)) {
    x.class <- sapply(x, class)
  } else {
    x.class <- class(x)
  }

  if(!is.atomic(x.class) & mode(x.class)=="list") {
    x.class <- do.call(c, lapply(x.class, paste, collapse=", "))


  } else if(!is.data.frame(x) & length(x.class) > 1) {
    x.class <- paste(x.class, collapse=", ")
  }

  if(is.data.frame(x)) {
    result <- data.frame(mode = sapply(x, mode),
                         class=x.class,
                         storage.mode = sapply(x, storage.mode),
                         is.factor = sapply(x, is.factor))
  } else {
    result <- data.frame(mode = mode(x),
                         class = x.class,
                         storage.mode = storage.mode(x),
                         is.factor = is.factor(x),
                         row.names=deparse(substitute(x)))
  }

  return(result)
}

#' Return Estimated Mean and Range as Character String
#'
#' This is a very simple function, which allows for estimation of means and ranges, and formatting the result
#' as a character string. Significant digits may be specified, and the number of values from which the mean and range are computed
#' may be returned if \code{ssize} is \code{TRUE}.
#'
#' @usage meanRange(x, digits, ssize=TRUE, type="arithmetic")
#'
#' @param x A vector of numeric values.
#' @param digits The number of significant digits to report. The default is to report three digits.
#' @param ssize Boolean; should the number of observations from which mean and range are computed be returned?
#' @param type A character string corresponding to the type of mean to be estimated, i.e. \code{arithmetic}, \code{geometric}, or \code{harmonic}.
#'
#' @return A named numeric vector, with one element if \code{ssize} is \code{FALSE}, and two elements if it is \code{TRUE}.
#'
#' @examples
#'
#' @author Ryan Kyle, \email{ryan.kyle@mail.mcgill.ca}
#' @keywords utilities
#'
#' @export
#'
meanRange <- function(x, digits=3, ssize=TRUE, type="arithmetic") {
  # handle null case
  if(is.null(x)) return()

  # handle NA case
  else if (all(is.na(x))) return()

  if(type=="arithmetic") {
    # compute mean, format with digits, type conversion to char
    meanChar <- table1::signif_pad(mean(x, na.rm=TRUE), digits=digits)
  } else if(type=="geometric") {
    # estimate geometric mean instead -- will fail to work properly if x contains zeroes
    meanChar <- table1::signif_pad(exp(mean(log(x), na.rm=TRUE)), digits=digits)
  } else if(type=="harmonic") {
    # estimate geometric mean instead -- will fail to work properly if x contains zeroes
    meanChar <- table1::signif_pad(1/(mean(1/x, na.rm=TRUE)), digits=digits)
  }

  # now proceed similarly with range
  rangeValues <- table1::signif_pad(range(x, na.rm=TRUE), digits=digits)

  # remove pesky space if present
  rangeValues <- gsub(" ", "", rangeValues)

  # now build the interval as character string
  rangeChar <- paste0("(", rangeValues[1], ", ", rangeValues[2], ")")

  # do we need to output sample size here -- omitted if FALSE
  if(!ssize) {
    result <- paste0(meanChar, " ", rangeChar)
    names(result) <- "Mean_Range"
  } else if(ssize==TRUE) {
    n <- as.character(sum(!is.na(x)))
    result <- c(n, paste0(meanChar, " ", rangeChar))
    names(result) <- c("N", "Mean_Range")
  }
  return(result)
}

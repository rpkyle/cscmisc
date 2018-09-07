#' Compute Confidence Limits Based on One-Sample t-Test
#'
#' This function emulates the behaviour of SAS's MEANS procedure, when specifying CLM. This is a convenience
#' wrapper for the \code{\link{t.test}} function, primarily useful for plotting purposes.
#'
#' @usage clmest(datavec, conf.level = 0.95)
#'
#' @param datavec Numeric. Vector of numeric values.
#' @param conf.level Numeric. Confidence level required for t-test when estimating limits.
#'
#' @examples
#' counts <- c(18,17,15,20,10,20,25,13,12)
#' clmest(counts)
#'
#' @author Ryan Kyle, \email{ryan.kyle@mail.mcgill.ca}
#'
#' @references \url{https://v8doc.sas.com/sashtml/proc/z0608466.htm}
#' @keywords utilities
#'
#' @export
#'
clmest<- function(datavec, conf.level=0.95) {
  estimates <- t.test(datavec, conf.level = conf.level)
  return(c(mean=unname(estimates$estimate),
           lclm=estimates$conf.int[1],
           uclm=estimates$conf.int[2]))
}

#' Estimated GFR Using the Mayo Formula
#'
#' Computes estimated glomerular filtration rate (GFR) using the Mayo Clinic quadratic formula.
#'
#' @usage \code{egfr_mayo(..., scr.mg.dL, age.yr, is.female, scr.umol.L, scr.conversion.factor = 88.4)}
#'
#' @param ...	Ignored. This forces all parameters to be named, which is safer.
#' @param scr.mg.dL	Numeric. Serum creatinine in mg/dL.
#' @param age.yr Numeric. Age in years.
#' @param is.female	Logical. TRUE for females, FALSE for males.
#' @param scr.umol.L	Numeric. Alternative specification of serum creatinine in umol/L.
#' @param scr.conversion.factor	Numeric. A factor for converting serum creatinine from umol/L to mg/dL.
#'
#' @details All arguments can be vectors.
#'
#' @author Ryan Kyle, \email{ryan.kyle@mail.mcgill.ca}
#'
#' @references \url{http://en.wikipedia.org/wiki/Creatinine_clearance}
#'
#' @examples
#' # With serum creatinine in mg/dL
#' egfr_mayo(scr.mg.dL=0.9, age.yr=53, is.female=FALSE)
#'
#' # With serum creatinine in umol/L
#' egfr_mayo(scr.umol.L=0.9*88.4, age.yr=53, is.female=FALSE)
#'
#' @keywords utilities
#'
#' @export
#'
egfr_mayo <- function (..., scr.mg.dL, age.yr, is.female, scr.umol.L,
                       scr.conversion.factor = 88.4)
{
  if (missing(scr.mg.dL) || is.null(scr.mg.dL)) {
    if (missing(scr.umol.L) || is.null(scr.umol.L)) {
      stop("One of scr.mg.dL and scr.umol.L must be present")
    }
    else {
      scr.mg.dL <- (scr.umol.L/scr.conversion.factor)
    }
  }
  else {
    if (!(missing(scr.umol.L) || is.null(scr.umol.L))) {
      stop("Only one of scr.mg.dL and scr.umol.L must be present")
    }
  }
  ifelse(scr.mg.dL < 0.8,
         exp(1.911+(5.249/0.8)-(2.114/(0.8^2))-0.00686*age.yr-(0.205*is.female)),
         exp(1.911+(5.249/scr.mg.dL)-(2.114/(scr.mg.dL^2))-0.00686*age.yr-(0.205*is.female))
  )
}

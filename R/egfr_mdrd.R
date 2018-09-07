#' Estimated GFR Using the MDRD Formula
#'
#' Computes estimated glomerular filtration rate (GFR) using the Modification of Diet in Renal Disease (MDRD) formula.
#' 
#' @usage \code{egfr_mdrd(..., scr.mg.dL, age.yr, is.female, is.black, bun.mg.dL=NULL, albumin.g.dL=NULL, scr.umol.L, scr.conversion.factor = 88.4)}
#' 
#' @param ...	Ignored. This forces all parameters to be named, which is safer.
#' @param scr.mg.dL	Numeric. Serum creatinine in mg/dL.
#' @param age.yr Numeric. Age in years.
#' @param is.female	Logical. TRUE for females, FALSE for males.
#' @param is.black Logical. TRUE if race is 'black', FALSE otherwise.
#' @param bun.mg.dL	Numeric. Blood urea nitrogen (BUN) concentrations in mg/dL (optional).
#' @param albumin.g.dL	Numeric. Albumin concentration in g/dL (optional).
#' @param scr.umol.L	Numeric. Alternative specification of serum creatinine in umol/L.
#' @param scr.conversion.factor	Numeric. A factor for converting serum creatinine from umol/L to mg/dL.
#' @param IDMS Logical. TRUE if IDMS correction should be used.
#' 
#' @details All arguments can be vectors. Albumin and BUN are optional. If they are specified, a more elaborate version of the MDRD equation is used.
#' 
#' @author Ben Rich, \email{benjamin.rich@certara.com}
#' 
#' @references \url{http://en.wikipedia.org/wiki/Creatinine_clearance}
#' @seealso \link{crcl.cg}
#' 
#' @examples
#' # With serum creatinine in mg/dL
#' egfr_mdrd(scr.mg.dL=0.9, age.yr=53, is.female=FALSE, is.black=FALSE)
#'
#' # With serum creatinine in umol/L
#' egfr_mdrd(scr.umol.L=0.9*88.4, age.yr=53, is.female=FALSE, is.black=FALSE)
#' 
#' @keywords utilities
#'
#' @export
#'
egfr_mdrd <- function (..., scr.mg.dL, age.yr, is.female, is.black, bun.mg.dL = NULL, 
                        albumin.g.dL = NULL, scr.umol.L, scr.conversion.factor = 88.4, IDMS=FALSE) 
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
  if (!is.null(bun.mg.dL) && !is.null(albumin.g.dL)) {
    170 * scr.mg.dL^(-0.999) * age.yr^(-0.176) * (1.18)^is.black * 
      (0.762)^is.female * bun.mg.dL^(-0.17) * albumin.g.dL^(0.318) * (ifelse(IDMS, (175/186), 1))
  }
  else {
    186 * scr.mg.dL^(-1.154) * age.yr^(-0.203) * (1.212)^is.black * 
      (0.742)^is.female  * (ifelse(IDMS, (175/186), 1))
  }
}
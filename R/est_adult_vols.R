#' Estimate Adult Fluid Volumes
#'
#' Computes estimated total body water, extracellular and intracellular fluid volumes for adults.
#'
#' @usage \code{est_adult_vols(..., wt.kg, wt.lb, is.female, return.df)}
#'
#' @param ...	Ignored. This forces all parameters to be named, which is safer.
#' @param wt.kg	Numeric. Weight in kilograms.
#' @param wt.lb	Numeric. Weight in pounds.
#' @param is.female	Logical. TRUE for females, FALSE for males.
#' @param return.df Logical. TRUE to return \code{data.frame}, FALSE to return \code{list}.
#'
#' @details All arguments can be vectors. By default, a \code{list} is returned, though a
#'          \code{data.frame} may be requested instead.
#'
#'          The formula used is simple, and derived from human physiology references:
#'
#'          Total body water (tbw) = 0.5 * wt.kg (females), 0.6 * wt.kg (males)
#'          Extracellular (ECC) volume (litres) = tbw * (1/3)
#'          Intracellular (ICF) volume (litres) = tbw * (2/3)
#'          Interstitial (ISF) volume (litres) = tbw * (3/4)
#'          Plasma volume (litres) = tbw * (1/4)
#'
#' @author Ryan Kyle, \email{ryan.kyle@mail.mcgill.ca}
#'
#' @examples
#' # For a male weighing 65kg, estimated total body water is 39L, ECC volume is 13L
#' est_adult_vols(wt.kg=65, is.female=FALSE)
#'
#' # For a female weighing 50kg, estimated total body water is 25L, ECC volume is 8.33L
#' est_adult_vols(wt.kg=50, is.female=TRUE)
#'
#' @keywords utilities
#'
#' @export
#'
est_adult_vols <- function (..., wt.kg, wt.lb, is.female, return.df=FALSE)
{
  if (missing(wt.kg) || is.null(wt.kg)) {
    if (missing(wt.lb) || is.null(wt.lb)) {
      stop("One of wt.kg or wt.lb must be present")
    }
    else {
      wt.kg <- wt.lb * 0.45359237
    }
  }

  if(length(wt.kg) != length(is.female)) {
    stop("All vectors passed to function must have equivalent length.")
  }

  tbw    <- ifelse(is.female, wt.kg * 0.5, wt.kg * 0.6)
  ecf    <- tbw * (1/3)
  icf    <- tbw * (2/3)
  isf    <- ecf * (3/4)
  plasma <- ecf * (1/4)

  result <- data.frame("sex"=ifelse(is.female, "Female", "Male"),
                         "weight.kg"=wt.kg,
                         "volume.units"="litres",
                         "total.body.water.volume"=tbw,
                         "extracellular.fluid.volume"=ecf,
                         "intracellular.fluid.volume"=icf,
                         "interstitial.fluid.volume"=isf,
                         "plasma.volume"=plasma)

  if(return.df) {
    return(result)
  }
  return(split(result, seq(nrow(result))))
}

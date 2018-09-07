#' Generate Intensive Sampling Times for mrgsolve Conditional on Dose Times
#'
#' The \code{sample_window} function uses dose administration times to generate additional observations for insertion
#' into a mrgsolve dataset. The more intensive sampling around drug doses may help more accurately estimate quantities
#' such as AUC, while avoiding potentially time-consuming oversampling at other times.
#'
#' @param dosetime Numeric. Vector of dose administration times.
#' @param interval Numeric. A single value corresponding to the width of the sampling interval.
#' @param sampfreq Numeric. The frequency of sampling (in fractions of an hour).
#' @param include Logical. Should dose times be included for sampling? Default is to avoid resampling at dose times.
#' @param before Logical. Should additional samples be taken before dose times, or only after a dose is administered?
#' @param negtime Logical. Should negative times be allowed? Default is to disallow negative observation times.
#'
#' @author Ryan Kyle, \email{ryan.kyle@mail.mcgill.ca}
#'
#' @keywords utilities
#'
#' @export
#'
sample_window <- function(dosetime, interval, sampfreq, include=FALSE, before=FALSE, negtime=FALSE) {
  after <- vecseq(dosetime, dosetime + interval, by=sampfreq)
  if(before) {
    before <- vecseq(dosetime - interval, dosetime-sampfreq, by=sampfreq)
    timeRange <- c(before, after)
  } else timeRange <- after

  if(negtime) {
    if(include) return(timeRange)
    else result <- timeRange[timeRange != dosetime]
  } else {
    if(include) return(timeRange[timeRange >= 0])
    else result <- timeRange[timeRange != dosetime & timeRange > 0]
  }

  return(sort(result))
}

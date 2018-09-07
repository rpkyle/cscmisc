#' Create Template Data Sets for mrgsolve, Allowing for Varying Sampling Intervals
#'
#' This function shares many features with \code{expand.ev} in the mrgsolve package,
#' but offers additional features.
#'
#' For intensive sampling around dose times, the \code{dense} option may be requested.
#' The user has control over the sampling frequency (times per hour),
#' as well as the width of the sampling window (time before/after dosing). \code{expand_obs}
#' calls the \code{sample_window} function to generate the additional observations before
#' or after dose times.
#
#' The resulting dataset includes the number of days, as well as the time of
#' day for the observation, as calculated from the start of the simulated
#'
#' Currently, the function assumes the dose variable is called \code{DOSE}, and the
#' time variable is called \code{TIME}.
#'
#' @usage expand_obs(..., indata, dense, units, duration, sampling.int, sampling.freq)
#'
#' @param ... passed to \code{\link{expand.grid}}
#' @param indata input data set
#' @param dense Boolean; if \code{TRUE}, sample more intensively around dose times. Otherwise, use consistent intervals only.
#' @param units Character string specifying units in minutes, hours, days, weeks, or years.
#' @param duration Numeric value specifying maximum length of simulations as specified in \code{units}.
#' @param sampling.int Numeric value specifying width of sampling interval around dose times in \code{units}.
#' @param sampling.freq Numeric value specifying sampling frequency for \code{mrgsim} in \code{units}
#' @param cmt Numeric value specifying compartment in which to dose; default is 1.
#'
#' @return A data frame ordered by subject, and then by time.
#'
#' @author Ryan Kyle, \email{ryan.kyle@mail.mcgill.ca}
#' @keywords utilities
#'
#' @export
#'
expand_obs <- function(...,
                       indata,
                       dense=TRUE,
                       units="hours",
                       duration=NA,
                       sampling.int=1,
                       sampling.freq=0.25) {
  if(is.na(duration)) stop("maximum time for generated observations (i.e. duration) must be specified.")

  if(units=="minutes") {
    maxtime <- 1/duration
  } else if(units=="hours") {
    maxtime <- duration
  } else if(units=="days") {
    maxtime <- (duration * 24) - 1
  } else if(units=="weeks") {
    maxtime <- (duration * 24 * 7) - 1
  } else if(units=="years") {
    maxtime <- (duration * 24 * 365) - 1
  }

  result <- expand.grid(ID=indata$ID[1],
                        EVID=0,
                        CMT=0,
                        TIME=seq(1, maxtime, 1),
                        AMT=0)

  result$DOSE <- indata$DOSE[1]
  result$DAY <- floor(result$TIME/24+1)
  result$TOD <- result$TIME %% 24

  if(dense) {
    addl   <- expand.grid(ID=indata$ID[1],
                          EVID=0,
                          CMT=0,
                          TIME=sample.window(indata$TIME,
                                             include=FALSE,
                                             before=TRUE,
                                             negtime=FALSE,
                                             interval=sampling.int,
                                             sampfreq=sampling.freq),
                          AMT=0)
    addl$DOSE <- indata$DOSE[1]
    addl$DAY <- floor(addl$TIME/24+1)
    addl$TOD <- addl$TIME %% 24
  }

  doses <- data.frame(ID=indata$ID[1],
                      EVID=1,
                      CMT=cmt,
                      TIME=indata$TIME,
                      AMT=indata$DOSE,
                      DOSE=indata$DOSE,
                      DAY=floor(indata$TIME/24+1),
                      TOD=indata$TIME %% 24)

  if(exists("addl")) result <- rbind(result, doses, addl)
  else result <- rbind(result, doses)

  return(result[order(result$TIME),])
}

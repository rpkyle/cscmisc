#' Compute Dose Frequency for Longitudinal Analyses in Long Format, with Handling for NONMEM-compliant data.
#'
#' This function produces tabular output which may be helpful for comparing dosing frequencies by individual in exploratory analyses.
#'
#' @param dosedata A data frame, including individual-level data organized with a unique key for each subject (\code{idvar}), and a secondary key corresponding to observed time (\code{timevar}). Neither should be missing.
#' @param idvar A unique, subject-level ID which may identify individual participants in the data set.
#' @param dosevar A character string specifying the column containing dose amount information.
#' @param timevar A character string specifying the column containing time information, assumed to be days since start of observation.
#' @param nmdata Boolean; if \code{TRUE}, assume data are in NONMEM-like format, and subset to rows containing \code{EVID==1}.
#'
#' @return A data frame, in which rows correspond to unique values of \code{idvar} and columns correspond to \code{qd}, \code{bid}, \code{tid}, and \code{qid} dosing.
#'
#' @author Ryan Kyle, \email{ryan.kyle@certara.com}
#' @keywords utilities
#'
#' @export
#'
doseFreq <- function(dosedata,
                     dosevar="DOSE",
                     timevar="DAY",
                     idvar="ID",
                     nmdata=FALSE) {

  dosedata <- as.data.frame(dosedata)
  if(nmdata) {
    dosedata <- subset(dosedata, EVID==1)
  }

  dosecol <- match(dosevar, names(dosedata))
  idcol <- match(idvar, names(dosedata))
  timecol <- match(timevar, names(dosedata))

  dosebyid <- split(dosedata, dosedata[,idcol])

  computeFreq <- function(inddata) {
    dosedays <- with(inddata, table(DOSE, DAY))
    dosedays <- t(apply(dosedays, 1, function(x) table(factor(x, levels=c(1:4), labels=c("qd", "bid", "tid", "qid")))))
    return(data.frame(ID=inddata[1,idcol], DOSE=rownames(dosedays), dosedays))
    return(dosedays)
  }

  result <- lapply(dosebyid, computeFreq)
  return(do.call(rbind, result))
}

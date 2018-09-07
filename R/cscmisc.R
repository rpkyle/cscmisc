#' cscmisc: Miscellaneous Tools for Data Manipulation and Simulation
#'
#' The {cscmisc} package is a collection of assorted functions. There's not much
#' here for the moment, but that may change with time.
#' 
#' @section Functions:
#' \describe{
#' \item{\link{addchar}}{Insert Characters Into an Existing String}
#' \item{\link{adjust_dtc}}{Add or Subtract Time from a Date-Time Object}
#' \item{\link{clmest}}{Compute Confidence Limits Based on One-Sample t-Test}
#' \item{\link{col_info}}{Produce an Object Detail Table}
#' \item{\link{difftime_dtc}}{Time Differences for Date/Time Objects}
#' \item{\link{doseFreq}}{Compute Dose Frequency for Longitudinal Analyses in Long Format, with Handling for NONMEM-compliant data.}
#' \item{\link{dt2dttm}}{Create Date/Time Object}
#' \item{\link{egfr_mayo}}{Estimated GFR Using the Mayo Formula}
#' \item{\link{egfr_mdrd}}{Estimated GFR Using the MDRD Formula}
#' \item{\link{est_adult_vols}}{Estimate Adult Fluid Volumes}
#' \item{\link{expand_obs}}{Create Template Data Sets for mrgsolve, Allowing for Varying Sampling Intervals}
#' \item{\link{f2n}}{Convert Factor to Numeric}
#' \item{\link{interleave}}{Interleave Two Vectors}
#' \item{\link{in_interval}}{Determine Whether Numeric Value Lies Within Specified Interval}
#' \item{\link{meanRange}}{Return Estimated Mean and Range as Character String}
#' \item{\link{medfind}}{Complete, Partial, and Fuzzy Matches for Medication (and other) Names}
#' \item{\link{numextract}}{Extract Numeric Values from Character Strings}
#' \item{\link{refactor}}{Rename and Collapse Existing Factor Levels}
#' \item{\link{rm_ws}}{Completely or Selectively Clear the R Workspace of Objects}
#' \item{\link{sample_window}}{Generate Intensive Sampling Times for mrgsolve Conditional on Dose Times}
#' \item{\link{sortlevels}}{Sort and Format Levels for Factoring}
#' \item{\link{source_verbose}}{Verbosely Read R Code from a File, a Connection, or Expressions}
#' \item{\link{timepart_dtc}}{Extract Time From Date/Time Objects}
#' \item{\link{toTitleCase_always}}{Convert All Text to Title Case}
#' \item{\link{vecseq}}{Vectorized Sequence Generation}
#' }
#'
#' @docType package
#' @name cscmisc
NULL
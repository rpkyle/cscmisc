#' Complete, Partial, and Fuzzy Matches for Medication (and other) Names
#'
#' This is a convenience function for identifying drug names and other strings in columns of data frames. Eventually support for RxNorm/UMLS
#' searching will be added, but for now there is no cross-referencing between search strings and generic or drug trade names.
#'
#' For applications involving searches of recorded medication data, the function enables easy and efficient searching while handling inconsistent
#' data entry -- accents and diacritic marks are removed using \code{\link{stri_trans_general}} from the \code{\link{stringi}} package, case is ignored, and
#' unusual or incorrect spelling of names is handled to a degree by fuzzy matching with \code{\link{agrep}}.
#'
#' @usage medfind(data, field, string, fuzzy=FALSE, distance=0.1)
#'
#' @param data A data frame, containing medication information in one or more columns.
#' @param field A character string identifying the column in which medication information is stored.
#' @param string A character string, partial or complete, to match in \code{field}.
#' @param fuzzy Logical. If \code{TRUE}, use approximate string matching with the generalized Levenshtein edit distance, as provided by \code{\link{agrep}}.
#' @param distance Maximum distance allowed for a match. Expressed either as integer, or as a fraction of the pattern length times the maximal transformation cost (will be replaced by the smallest integer not less than the corresponding fraction).
#'
#' @return A vector of unique character strings corresponding to the identified matches of \code{string} in \code{field}.
#'
#' @examples
#'
#' # The mednames table is a simple, completely simulated record of participant IDs, treatment dates, and medication names.
#' # This example results in a single hit, for the exact match.
#' medfind(mednames, field = "medication", string = "cholecalciferol")
#'
#' # Using fuzzy matching, we identify 8 matches -- several of which are spelled improperly or contain diacritic marks.
#' medfind(mednames, field = "medication", string = "cholecalciferol", fuzzy=TRUE)
#'
#' # By increasing the distance value, we find two more matches, but both are for ergocalciferol -- a slightly different compound.
#' medfind(mednames, field = "medication", string = "cholecalciferol", fuzzy=TRUE, distance=0.25)
#'
#' @author Ryan Kyle, \email{ryan.kyle@mail.mcgill.ca}
#' @keywords utilities
#'
#' @export
#'
medfind <- function(data, field, string, fuzzy=FALSE, distance=0.1) {
  colnum <- match(field, names(data))
  drugNames <- names(table(data[colnum]))

  clean_string <- stringi::stri_trans_general(string, "latin-ascii")
  clean_field <- stringi::stri_trans_general(drugNames, "latin-ascii")

  clean_string <- tolower(clean_string)
  clean_field <- tolower(clean_field)

  if(fuzzy) {
    indices <- agrep(pattern = clean_string, clean_field, max.distance=distance)
  } else indices <- grep(pattern = clean_string, clean_field)

  results <- names(table(data[colnum]))[indices]
  return(results)
}

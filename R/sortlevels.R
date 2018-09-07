#' Sort and Format Levels for Factoring
#'
#' This function will reformat and order visit names for use in factors; it also allows modifying case and excluding some levels.
#'
#' @usage sortlevels(all.visits, pre.levels, excluded.levels, title.case=TRUE)
#'
#' @param all.visits A vector of all visit names to sort and format.
#' @param pre.levels A vector of visit levels that should precede the rest (e.g. screening or pre-study visits) in ordering.
#' @param excluded.levels A vector of visit names that should be excluded from the factor levels.
#' @param title.case Logical. Should the names of the factor levels use title case?
#'
#' @return A vector of visit levels, sorted and formatted, potentially with exclusions.
#'
#' @author Ben Rich, \email{benjamin.rich@certara.com} with edits by Ryan Kyle, \email{ryan.kyle@mail.mcgill.ca}
#' @keywords utilities
#'
#' @export
#'
sortlevels <- function(all.visits, pre.levels=NA, excluded.levels=NA, title.case=TRUE) {
  if(is.factor(all.visits)) {
    x <- sort(levels(all.visits))
  } else {
    x <- sort(unique(all.visits))
  }
  if(title.case) {
    x <- toTitleCase_always(x)
    if(!anyNA(pre.levels)) pre.levels <- toTitleCase_always(pre.levels)
    if(!anyNA(excluded.levels)) excluded.levels <- toTitleCase_always(excluded.levels)
  }
  if(!anyNA(excluded.levels)) {
    x <- setdiff(x, excluded.levels)
  }
  if(!anyNA(pre.levels)) {
    x <- setdiff(x, pre.levels)
  }
  df <- suppressWarnings(as.data.frame(cbind(x, do.call(rbind, strsplit(x, " ")))))
  df$V3 <- suppressWarnings(as.numeric(as.character(df$V3)))
  df <- df[order(df$V2, df$V3),]
  if(!anyNA(pre.levels)) {
    c(pre.levels, as.character(df$x))
  } else as.character(df$x)
}

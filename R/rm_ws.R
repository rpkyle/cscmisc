#' Completely or Selectively Clear the R Workspace of Objects
#'
#' This is a convenience function for cleaning the R workspace; either all objects can be removed, or only a subset of them.
#'
#' If exclusions are specified, but none are matched, the function will abort for safety reasons. This is intended to prevent
#' the user from inadvertently clearing the workspace using stale or poorly formatted arguments.
#'
#' @usage rm.ws(..., exclude, verbose=FALSE, reqmatch=TRUE)
#'
#' @param ... Arguments to be passed directly to \code{rm}.
#' @param exclude Boolean; should any objects be retained when the workspace is cleared?
#' @param verbose Boolean; should output be written to console listing the deleted/retained objects?
#' @param reqmatch Boolean; should \code{rm.ws} require that at least one exclusion matches?
#'
#' @return None
#'
#' @author Ryan Kyle, \email{ryan.kyle@mail.mcgill.ca}
#' @keywords utilities
#'
#' @export
#'
rm_ws <- function(..., exclude=NA, verbose=FALSE, reqmatch=TRUE) {
  wslist_before <- ls(.GlobalEnv)

  if(length(wslist_before) == 0) stop("nothing removed by rm_ws; no objects currently in workspace.")

  if(!anyNA(exclude) & length(intersect(exclude, wslist_before)) == 0 & reqmatch==TRUE) {
    stop("No items in exclusions list are in workspace -- if this is really what you want, use argument reqmatch=FALSE.")
  } else if(!anyNA(exclude)) {
    rmlist <- setdiff(wslist_before, exclude)
  } else rmlist <- wslist_before

  rm(list=rmlist, envir=.GlobalEnv, ...)

  wslist_after <- ls(.GlobalEnv)

  if(verbose) {
    if(length(wslist_after) == 0) cat("All objects removed; workspace is empty.\n")
    else if(!anyNA(exclude)) {
      cat("+ Workspace after removing objects:\n", paste0("   ", wslist_after, "\n"))
      cat("- Removed from workspace:\n", paste0("   ", rmlist, "\n"))
    }
  }
}

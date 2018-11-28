#' Verbosely Read R Code from a File, a Connection, or Expressions
#'
#' \code{source_verbose} behaves identically to \code{\link{source}}, but prints a list of objects added to or removed from
#' the chosen environment to the console. The function is a simple wrapper; if the workspace is unchanged, nothing is printed.
#'
#' @param ... Arguments to pass to \code{source}.
#'
#' @author Ryan Kyle, \email{ryan.kyle@mail.mcgill.ca}
#' @keywords utilities
#'
#' @export
#'
source_verbose <- function(..., envir=.GlobalEnv) {
  wslist_before <- ls(envir=envir)
  source(...)
  wslist_after <- ls(envir=envir)

  if(any(!(wslist_after %in% wslist_before))) {
    cat("+ Added to workspace:\n", paste0("   ", wslist_after[(!(wslist_after %in% wslist_before))], "\n"))
  }
  if(any(!(wslist_before %in% wslist_after))) {
    cat("- Removed from workspace:\n", paste0("   ", wslist_before[(!(wslist_before %in% wslist_after))], "\n"))
  }
}

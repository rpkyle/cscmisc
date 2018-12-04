#' Detach All Packages 
#'
#' This function will attempt to detach all packages loaded by the user during the current R session. 
#'
#' @usage detach_all(verbose=TRUE, unload=TRUE) 
#' 
#' @param verbose Should a message be printed to the console naming all the detached packages? \code{TRUE} or \code{FALSE}.
#' 
#' @param unload Should the packages be unloaded from the namespace? \code{TRUE} or \code{FALSE}. 
#'
#' @details It's probably smarter to restart R, but being able to detach and unload all packages can be handy at times. 
#'
#' @author Ryan Kyle, \email{ryan.kyle@mail.mcgill.ca}
#' @keywords utilities
#'
#' @export
#'
detach_all <- function(verbose=TRUE, unload=TRUE) {
  pkgList <- names(sessionInfo()$otherPkgs)
  if(!is.null(pkgList)) {
    if(verbose) {
      cat("Detaching:", paste(pkgList, collapse=", "), "\n")
    }
    invisible(lapply(
      paste0('package:',
            pkgList),
      detach,
      character.only=TRUE,
      unload=unload))    
  } else(stop("No loaded packages to detach!"))
}

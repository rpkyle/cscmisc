detach_all <- function(verbose=TRUE) {
  pkgList <- names(sessionInfo()$otherPkgs)
  if(!is.null(pkgList)) {
    if(verbose) {
      cat("Detaching:", paste(pkgList, collapse=", "), "\n")
    }
    invisible(lapply(
      paste('package:',
            pkgList,
            sep=""),
      detach,
      character.only=TRUE,
      unload=TRUE))    
  } else(stop("No loaded packages to detach!"))
}

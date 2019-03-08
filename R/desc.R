#' Quickly view a package DESCRIPTION 
#'
#' This is a simple convenience function to easily view a
#' package DESCRIPTION file for a given R package. Since
#' package maintainers often omit a generic help file
#' including  _PACKAGE, typing help("somePackage") is not
#' always successful. 
#'
#' @usage desc(package)
#'
#' @param package A character string corresponding to a
#' valid R package name. 
#'
#' @author Ryan Kyle, \email{ryan.kyle@mail.mcgill.ca}
#'
#' @keywords utilities
#'
#' @export
#'
desc <- function(package) {
  port <- tools::startDynamicHelp(NA)
  if (port <= 0L) 
    return(library(help = package, lib.loc = lib.loc, 
                   character.only = TRUE))
  browser <- if (.Platform$GUI == "AQUA") {
    get("aqua.browser", envir = as.environment("tools:RGUI"))
    }
  else getOption("browser")
  browseURL(paste0("http://127.0.0.1:", port, "/library/", 
                   package, "/DESCRIPTION"), browser)
  return(invisible())
} 


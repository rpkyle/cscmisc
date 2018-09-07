#' Insert Characters into an Existing String
#'
#' Given a string, and a starting position, this function will enlarge
#' the string to include additional characters.
#'
#' @usage \code{addchar(string, characters, position)}
#'
#' @param string Character. A string of class \code{character}.
#' @param characters Character. A string containing the additional characters to insert.
#' @param position Numeric. The position within \code{string} where
#'                 \code{characters} should be added.
#'
#' @examples
#' # Returns "The quick brown fox jumps over the lazy dog."
#' orig_string <- "The brown fox jumps over the lazy dog."
#' new_string  <- "quick "
#'
#' addchar(orig_string, new_string, 4)
#'
#' @author Ryan Kyle, \email{ryan.kyle@mail.mcgill.ca}
#'
#' @keywords utilities
#'
#' @export
#'
addchar <- function(string, characters, position) {
  prestring  <- substring(string, 1, position)
  poststring <- substring(string, position + 1)
  return(paste0(prestring, characters, poststring))
}

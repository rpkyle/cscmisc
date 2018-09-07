#' Always Convert Text to Title Case
#'
#' Convert a character vector to title case, less carefully than \code{toTitleCase} does. Unlike \code{toTitleCase}, requires loading no packages (other than this one).
#'
#' @param text A character string to be converted to title case.
#'
#' @details Modified by Ryan Kyle, based on function from \url{https://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string}.
#'
#' @return A character vector of the same length as \code{text}, without names.
#'
#' @author Andrie de Vries
#'
#' @keywords utilities
#'
#' @export
#'
toTitleCase_always <- function(text) {
  x <- tolower(text)
  s <- sapply(x, strsplit, split="/| ", simplify=TRUE)

  titleCase <- function(s) {
    paste0(toupper(substring(s, 1,1)), substring(s, 2), collapse=" ")
  }

  result <- lapply(s, titleCase)
  unlist(unname(result))
}

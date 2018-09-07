#' Extract Numeric Values from Character Strings
#'
#' Given a character string, this function will attempt to extract digits and return the result as
#' a numeric value.
#'
#' @param string A single string of \code{class} string to parse for digits.
#' @param sequence A second character string, matching one of the following: \code{"first"}, \code{"last"}, \code{"collapse"}, or \code{"midpoint"}.
#'
#' @details All functions used are available in base R; no additional packages are required.
#'          If one matching sequence is identified, but the \code{sequence} argument is \code{"midpoint"}
#'          or \code{"collapse"}, the function attempts to return a "safe" value. In this case, the only
#'          numeric match is returned. If no matches are found, the function returns \code{numeric()}.
#'
#' @return Numeric value(s) occurring in \code{string}, or the midpoint of the first and last digits
#'         within the string.
#'
#' @author Ryan Kyle, \email{ryan.kyle@mail.mcgill.ca}
#'
#' @examples
#' example_string1 <- "12-15 HOURS"
#' example_string2 <- "DAY -1"
#'
#' # Returns 12.
#' numextract(example_string1)
#' numextract(example_string1, sequence="first")
#'
#' # Returns -15, a negative numeric value.
#' numextract(example_string1, sequence="last")
#'
#' # Returns 1215, compressing two sequences into one.
#' numextract(example_string1, sequence="collapse")
#'
#' # Returns 13.5, which is the midpoint of 15 and 12
#' (assumes second sequence does not correspond to negative numeric value).
#' numextract(example_string1, sequence="midpoint")
#'
#' # All return -1
#' numextract(example_string2)
#' numextract(example_string2, sequence="first")
#' numextract(example_string2, sequence="last")
#' numextract(example_string2, sequence="midpoint")
#' numextract(example_string2, sequence="collapse")
#'
#' @keywords utilities
#'
#' @export
#'
numextract <- function (string, sequence = "first")
{
  # convert factor to string
  string <- sapply(string, FUN = function(x) {
    ifelse("factor" %in% class(x), as.character(x), x)
  })

  # check to see if it's a character string with no digits
  string <- sapply(string, FUN = function(x) {
    ifelse(!(grepl("[[:digit:]]", x)), NA, x)
  })

  if (is.na(sequence))
    stop("sequence cannot be NA; please specify first, last, collapse, or midpoint (latter will compute mean of last and first).")
  if (sequence == "collapse") {
    # Allow retrieval of a negative number if only one digit sequence exists
    string <- sapply(string, FUN = function(x) {
      ifelse(lengths(regmatches(string, gregexpr("[[:digit:]]", string))) == 1,
             gsub("[^-\\d.]+", "", string, perl = TRUE),
             gsub("[^\\d.]+", "", string, perl = TRUE))
    })

    return(as.numeric(string))
  }
  if (sequence == "midpoint") {
    string <- sapply(string, FUN = function(x) {
      if(lengths(regmatches(x, gregexpr("[[:digit:]]", x))) == 1) {
        return(gsub("[^-\\d.]+", "", x, perl = TRUE))
      } else {
        first.pos <- regexpr("(\\-*\\d+\\.*\\d*)", x, perl = TRUE)
        last.pos <- regexpr("(\\d+\\.*\\d*)(?!.*\\d)", x, perl = TRUE)

        first.pos.out <- rep(NA, length(x))
        last.pos.out <- rep(NA, length(x))

        # Preserve NA values
        first.pos.out[!is.na(first.pos)] <- regmatches(x, first.pos)
        last.pos.out[!is.na(last.pos)] <- regmatches(x, last.pos)

        # Generate a table with the first and last matches by string
        numlist <- mapply(c, as.numeric(first.pos.out), as.numeric(last.pos.out), SIMPLIFY = TRUE)

        # Compute the mean of the two values
        result <- apply(numlist, MARGIN = 2, FUN=mean)
        return(result)
      }
    })
    return(as.numeric(string))
  }
  if (sequence == "first")
    pattern <- "(\\-*\\d+\\.*\\d*)"
  if (sequence == "last")
    pattern <- "(\\-*\\d+\\.*\\d*)(?!.*\\d)"
  hits <- regexpr(pattern, string, perl = TRUE)
  result <- rep(NA, length(string))
  result[!is.na(hits)] <- regmatches(string, hits)
  result <- as.numeric(result)
  return(result)
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/refactor.R
\name{refactor}
\alias{refactor}
\title{Rename and Collapse Existing Factor Levels}
\usage{
refactor(x, newlevels, includeNA = FALSE)
}
\arguments{
\item{x}{A vector.}

\item{newlevels}{A list of levels to use for refactoring, named according to the original levels of \code{x}.}

\item{includeNA}{Boolean; should \code{NA} values be included as a level, and possibly renamed?}
}
\value{
The functions returns \code{x}, with levels collapsed/renamed according to \code{newlevels}.
}
\description{
This function takes a vector object as input, and encodes (or re-encodes, as appropriate)
the vector as a factor with levels corresponding to newlevels, a list object.
}
\details{
This is merely a convenience function to simplify renaming and collapsing factor levels. Original
code written by Marek Szatkowski -- only difference here is slight change to handle \code{NA} values.
}
\examples{

\dontrun{
test$tpt <- refactor(test$tpt, list("1-2hr Post-dose"=c("1-2 HOUR POST DOS",
"1-2 HOUR POST DOSE", "1-2 HR POSTDOSE", "1 HOUR POST DOSE"), "6-10hr Post-dose"="6-10 HOUR POST DOSE"))

# To include NA as a factor level and reclassify:

test$tpt <- refactor(test$tpt, list("1-2hr Post-dose"=c("1-2 HOUR POST DOS", "1-2 HOUR POST DOSE",
"1-2 HR POSTDOSE", "1 HOUR POST DOSE"), "6-10hr Post-dose"="6-10 HOUR POST DOSE", "Pre-dose"=NA), includeNA=TRUE)
}

}
\references{
\url{https://stackoverflow.com/questions/10431403/idiom-for-ifelse-style-recoding-for-multiple-categories/10432263#10432263}
}
\author{
Marek Szatkowski
}
\keyword{utilities}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dpinput_map.R
\name{dirTree_build}
\alias{dirTree_build}
\title{Build directory tree}
\usage{
dirTree_build(flattened_dirTree)
}
\arguments{
\item{flattened_dirTree}{out of call to \code{dirTree_flatten}}
}
\value{
dirTree a similar to output of \code{dirTree_flatten} but with each
element enriched with tidy metadata including
sha1 and original path to each data
}
\description{
Receives a flattened directory tree from \code{dirTree_flatten} and
appends a metadata tibble to each element
}
\keyword{internal}

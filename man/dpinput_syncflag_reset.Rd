% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dpinput_map.R
\name{dpinput_syncflag_reset}
\alias{dpinput_syncflag_reset}
\title{Reset sync flag in the manifest}
\usage{
dpinput_syncflag_reset(input_map, input_id)
}
\arguments{
\item{input_map}{input_map made with \code{dpinput_map}}

\item{input_id}{a vector of character strings matching
\code{input_mapt$input_manifest$id}}
}
\value{
modified \code{input_map}
}
\description{
It updates input_map manifest and reverses the to_be_synced flag
state
}

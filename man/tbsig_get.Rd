% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{tbsig_get}
\alias{tbsig_get}
\title{Get sha1 signature for a table}
\usage{
tbsig_get(d)
}
\arguments{
\item{d}{data.frame}
}
\value{
tbsig a character string
}
\description{
This function is a wrapper around digest::sha1 to handle exotic column classes
}

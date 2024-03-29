% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{dpname_get}
\alias{dpname_get}
\title{Get Data Product Name}
\usage{
dpname_get(data_object)
}
\arguments{
\item{data_object}{Data Object. It anticipates project_name and branch_name to be attributes of this data object}
}
\value{
dp_name a character that will be tagged back as attribute to data_object
}
\description{
A helper function that build data product name (i.e. what the pin is going to be called)
}

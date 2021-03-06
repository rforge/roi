% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/constraints.R
\name{as.L_term}
\alias{as.L_term}
\title{Canonicalize the Linear Term}
\usage{
as.L_term(x, ...)
}
\arguments{
\item{x}{an R object.}

\item{...}{further arguments passed to or from other methods.}
}
\value{
an object of class \code{"simple_triplet_matrix"}
}
\description{
Canonicalize the linear term of a linear constraint.
  Objects from the following classes can be canonicalized:
  \code{"NULL"}, \code{"numeric"}, \code{"matrix"}, \code{"simple_triplet_matrix"}
  and \code{"list"}.
}
\details{
In the case of \code{lists} \code{"as.Q_term"} is applied to every element
  of the list, for \code{NULL} one can supply the optional arguments \code{"nrow"}
  and \code{"ncol"} which will create a \code{"simple_triplet_zero_matrix"}
  with the specified dimension.
}

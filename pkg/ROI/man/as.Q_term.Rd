% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/constraints.R
\name{as.Q_term}
\alias{as.Q_term}
\alias{as.Q_term.list}
\alias{as.Q_term.numeric}
\alias{as.Q_term.matrix}
\alias{as.Q_term.simple_triplet_matrix}
\alias{as.Q_term.NULL}
\title{Canonicalize the Quadraric Term}
\usage{
as.Q_term(x, ...)

\method{as.Q_term}{list}(x, ...)

\method{as.Q_term}{numeric}(x, ...)

\method{as.Q_term}{matrix}(x, ...)

\method{as.Q_term}{simple_triplet_matrix}(x, ...)

\method{as.Q_term}{`NULL`}(x, ...)
}
\arguments{
\item{x}{an R object.}

\item{...}{further arguments}
}
\value{
an object of class \code{"simple_triplet_matrix"}
}
\description{
Canonicalize the quadraric term of a quadratic constraint.
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

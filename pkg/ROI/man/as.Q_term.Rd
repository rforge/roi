% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/constraints.R
\name{as.Q_term}
\alias{as.Q_term}
\alias{as.Q_term.NULL}
\alias{as.Q_term.list}
\alias{as.Q_term.matrix}
\alias{as.Q_term.numeric}
\alias{as.Q_term.simple_triplet_matrix}
\title{Coerce an object to type \code{"Q_term"}.}
\usage{
as.Q_term(x, ...)

\method{as.Q_term}{list}(x, ...)

\method{as.Q_term}{numeric}(x, ...)

\method{as.Q_term}{matrix}(x, ...)

\method{as.Q_term}{simple_triplet_matrix}(x, ...)

\method{as.Q_term}{NULL}(x, ...)
}
\arguments{
\item{x}{an R object.}

\item{...}{further arguments passed to or from other methods
(currently ignored).}
}
\value{
an object of class \code{"simple_triplet_zero_matrix"}
}
\description{
The \code{"Q_term"} object represents the quadratic term of the \code{"Q_constraint"}.
  Objects from the following classes can be coerced to \code{"Q_term"}:
  \code{"NULL"}, \code{"numeric"}, \code{"matrix"}, \code{"simple_triplet_matrix"}
  and \code{"list"}.
}
\details{
In the case of \code{lists} \code{"as.Q_term"} is applied to every element
  of the list, for \code{NULL} one can supply the optional arguments \code{"nrow"}
  and \code{"ncol"} which will create a \code{"simple_triplet_zero_matrix"}
  with the specified dimension.
}

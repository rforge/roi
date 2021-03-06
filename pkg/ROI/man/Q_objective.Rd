% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/objective.R
\name{Q_objective}
\alias{Q_objective}
\alias{terms.Q_objective}
\alias{as.Q_objective}
\alias{variable.names.Q_objective}
\title{Quadratic Objective Function}
\usage{
Q_objective(Q, L = NULL, names = NULL)

\method{terms}{Q_objective}(x, ...)

as.Q_objective(x)

\method{variable.names}{Q_objective}(object, ...)
}
\arguments{
\item{Q}{a \eqn{n \times n} matrix with numeric entries representing the quadratic
part of objective function. Sparse matrices of class
\code{"simple_triplet_matrix"} can be supplied.}

\item{L}{a numeric vector of length \eqn{n}, where \eqn{n} is the
number of objective variables.}

\item{names}{an optional character vector giving the names of \eqn{x}
(row/column names of \eqn{Q}, column names of \eqn{L}).}

\item{x}{an R object.}

\item{...}{further arguments passed to or from other methods}

\item{object}{an R object.}
}
\value{
an object of class \code{"Q_objective"} which inherits
from \code{"objective"}.
}
\description{
A quadratic objective function is typically of the form
\deqn{\frac{1}{2} x^\top Qx + c^\top x} where \eqn{Q} is a (sparse) matrix
defining the quadratic part of the function and \eqn{c} is a
(sparse) vector of coefficients to the \eqn{n} defining the linear
part.
}
\author{
Stefan Theussl
}

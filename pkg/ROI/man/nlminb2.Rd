% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plugin_nlminb.R
\name{nlminb2}
\alias{nlminb2}
\title{Nonlinear programming with nonlinear constraints.}
\usage{
nlminb2(
  start,
  objective,
  eqFun = NULL,
  leqFun = NULL,
  lower = -Inf,
  upper = Inf,
  gradient = NULL,
  hessian = NULL,
  control = list()
)
}
\arguments{
\item{start}{numeric vector of start values.}

\item{objective}{the function to be minimized \eqn{f(x)}.}

\item{eqFun}{functions specifying equal constraints of the form
\eqn{h_i(x) = 0}. Default: \code{NULL} (no equal constraints).}

\item{leqFun}{functions specifying less equal constraints of the
form \eqn{g_i(x) <= 0}. Default: \code{NULL} (no less equal
constraints).}

\item{lower}{a numeric representing lower variable
bounds. Repeated as needed. Default: \code{-Inf}.}

\item{upper}{a numeric representing upper variable
bounds. Repeated as needed. Default: \code{Inf}.}

\item{gradient}{gradient of \eqn{f(x)}. Default: \code{NULL} (no
gradiant information).}

\item{hessian}{hessian of \eqn{f(x)}. Default: \code{NULL} (no
hessian provided).}

\item{control}{a list of control parameters. See
\code{\link[stats]{nlminb}()} for details. The parameter
\code{"scale"} is set here in contrast to
\code{\link[stats]{nlminb}()} .}
}
\value{
list()
}
\description{
This function was contributed by Diethelm Wuertz.
}
\examples{
## Equal constraint function
eval_g0_eq <- function( x, params = c(1,1,-1)) {
       return( params[1]*x^2 + params[2]*x + params[3] )
   }
eval_f0 <- function( x, ... ) {
       return( 1 )
   }


}
\author{
Diethelm Wuertz
}

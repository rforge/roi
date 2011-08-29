##' Optimization problem constructor
##'
##' @title Optimization Problem Constructor
##' @param objective an object inheriting from class \code{"objective"}.
##' @param constraints an object inheriting from class \code{"constraints"}.
##' @param bounds \code{NULL} (default) or a list with elements
##' \code{upper} and \code{lower} containing the indices and
##' corresponding bounds of the objective variables.  The default for
##' each variable is a bound between 0 and \code{Inf}.
##' @param types a character vector giving the types of the objective
##' variables, with \code{"C"}, \code{"I"}, and \code{"B"}
##' corresponding to continuous, integer, and binary, respectively, or
##' \code{NULL} (default), taken as all-continuous.  Recycled as
##' needed.
##' @param maximum a logical giving the direction of the
##' optimization. \code{TRUE} means that the objective is to maximize
##' the objective function, \code{FALSE} (default) means to minimize
##' it.
##' @return A list containing the optimal solution, with the following
##' components.
##' \item{solution}{the vector of optimal coefficients}
##' \item{objval}{the value of the objective function at the optimum}
##' \item{status}{an integer with status information about the
##' solution returned: 0 if the optimal solution was found, a non-zero
##' value otherwise}
##' \item{msg}{the status code and additional
##' information about the solution provided by the solver.}
##' @author Stefan Theussl
##' @export
OP <- function( objective, constraints, bounds = NULL, types = NULL,
  maximum = FALSE ) {
    structure(list(objective = as.objective(objective),
                   constraints = as.constraint(constraints),
                   bounds = bounds,
                   types = types,
                   maximum = maximum), class = "OP")
}

## FIXME: also consider objective function

print.OP <- function(x, ...){
    types <- c(L_constraint = "linear", Q_constraint = "quadratic", F_constraint = "nonlinear" )
    writeLines( sprintf("A mathematical programming problem with %d constraints of type %s.", length(constraints(x)),
                        paste(na.omit(types[class(constraints(x))]), collapse = ", ")) )
}

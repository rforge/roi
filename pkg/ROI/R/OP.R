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
OP <- function( objective, constraints = NULL, types = NULL, bounds = NULL,
  maximum = FALSE ) {
    structure(list(objective = as.objective(objective),
                   constraints = as.constraint(constraints),
                   bounds = bounds,
                   types = types,
                   maximum = maximum), class = "OP")
}

## FIXME: also consider objective function

##' @method print OP
##' @S3method print OP
print.OP <- function(x, ...){
    types <- c(L_constraint = "linear", Q_constraint = "quadratic", F_constraint = "nonlinear" )
    writeLines( sprintf("A mathematical programming problem with %d constraints of type %s.", length(constraints(x)),
                        paste(na.omit(types[class(constraints(x))]), collapse = ", ")) )

}

##' @export
as.OP <- function(x)
    UseMethod("as.OP")

##' @method as.OP OP
##' @S3method as.OP OP
as.OP.OP <- identity

##' @method as.OP numeric
##' @S3method as.OP numeric
as.OP.numeric <- function(x){
    OP( objective = x, constraints = NULL, bounds = NULL, types = NULL,
        maximum = FALSE )

##' @method as.OP default
##' @S3method as.OP default
as.OP.default <- function(x, ...)
    stop("Method not implemented.")

}

## OP_class <- function( x ){
##     x <- as.OP( x )
##     uniq_types <- if( is.null(types(x)) )
##         available_types()[1]
##     else unique(types(x))
##     signature <- list(

##                       )
##     c(sapply(available_objective_classes(),
##                           function(what) inherits(objective(x), what) ),
##                    sapply(available_constraint_classes(),
##                           function(what) inherits(constraints(x), what)),
##                    sapply(available_types(),
##                           function(what) what %in% uniq_types),
##                    bounds  = !is.null(bounds(x)),
##                    maximum = x$maximum
##     )
##     signature
## }

##' @export
OP_signature <- function( x ){
    x <- as.OP( x )
    uniq_types <- if( is.null(types(x)) )
        available_types()[1]
    else unique(types(x))
    ROI_make_signature( objective = names( available_objective_classes() )[ ROI:::available_objective_classes() %in% class(objective(x)) ],
                        constraints = names( available_constraint_classes() )[ ROI:::available_constraint_classes() %in% class(constraints(x)) ],
                        types = uniq_types,
                        bounds  = !is.null(bounds(x)),
                        maximum = x$maximum
                       )
}


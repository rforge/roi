################################################################################
## Package: ROI
## File:    objective.R
## Author:  Stefan Theussl
## Changed: 2012-09-04
################################################################################



################################################################################
## objective helper functions
################################################################################

available_objective_classes <- function()
    c( L = "L_objective", Q = "Q_objective", F = "F_objective" )

## the 'objective' class - a named list of coefficients of a polynom
## of degree 'k'.
## o k = 1: linear objective function (element L), numeric
## o k = 2: quadratic objective function (Q), matrix
## o k > 2: higher polynomial objective function (P_k), multidimensional array
## o Nonlinear objective function (F), function
## Note that for polynoms of degree k each element in the list contains the
## corresponding coefficients defining the polynom. E.g., c^\top x => L = c
## c^\top x + x^\top Q x => Q = Q, L = c, etc. ordering is always from highest
## to lowest degree.
.objective <- function( nobj, ... ){
    structure( list(...), nobj = nobj, class = "objective")
}

## get objective function from problem object
## returns a function!

##' Extract the objective function from its argument (typically ROI
##' objects) and return them.
##'
##' The default method assumes that the supplied R object is a list
##' where the element \code{objective} represents the objective
##' function. The extractet element is then coerced to a function.
##' @title Extract Objective Functions
##' @param x an object used to select the method.
##' @return a function inheriting from \code{"objective"}.
##' @author Stefan Theussl
##' @export
objective <- function( x )
    UseMethod( "objective" )

##' @nord
##' @method objective default
##' @S3method objective default
objective.default <- function( x )
    as.function( x$objective )

##' Coerces objects of type \code{"objective"}.
##'
##' @title Objective Function Utilities
##' @param x an R object.
##' @return an object of class \code{"objective"}.
##' @author Stefan Theussl
##' @export
as.objective <- function( x )
  UseMethod("as.objective")

##' @nord
##' @method as.objective default
##' @S3method as.objective default
as.objective.function <- function( x ){
    if( inherits(x, "Q_objective", which = TRUE) == 2 )
        return( as.Q_objective( x ) )
    if( inherits(x, "L_objective", which = TRUE) == 2 )
        return( as.L_objective( x ) )
    stop("Not implemented.")
}

##' @nord
##' @method as.objective default
##' @S3method as.objective default
as.objective.default <- function( x )
  as.L_objective( x )

##' @nord
##' @method as.objective objective
##' @S3method as.objective objective
as.objective.objective <-
    identity

##' @nord
##' @method length objective
##' @S3method length objective
length.objective <- function( x )
    attr( as.objective(x), "nobj" )

##' @nord
##' @method terms function
##' @S3method terms function
terms.function <- function( x, ... ){
    if( inherits(x, "L_objective") )
        return( terms(as.L_objective(x)) )
    if( inherits(x, "Q_objective") )
        return( terms(as.Q_objective(x)) )
    NA
}

##' @nord
##' @method terms L_objective
##' @S3method terms L_objective
terms.L_objective <- function( x, ... )
  list( L = x$L )

##' @nord
##' @method terms Q_objective
##' @S3method terms Q_objective
terms.Q_objective <- function( x, ... )
  list( Q = x$Q, L = x$L )




###############################################################
## linear objectives
###############################################################

## Linear objective function (class 'L_objective')
## of type c^\top x, where c is a vector of coefficients

##' A linear objective function is typically of the form \eqn{c^\top
##' x} where \eqn{c} is a (sparse) vector of coefficients to the
##' \eqn{n} objective variables \eqn{x}.
##'
##' @title Linear Objective Function
##' @param L a numeric vector of length \eqn{n} or an object of class
##' \code{"simple_triplet_matrix"} (or coercible to such) with dimension \eqn{1 \times n},
##' where \eqn{n} is the number of objective variables. Names will be
##' preserved and used e.g., in the print method.
##' @author Stefan Theussl
##' @export
L_objective <- function( L ) {
    obj <- Q_objective( Q = NULL, L = L )
    class( obj ) <- c( "L_objective", class(obj) )
    obj
}

##' @nord
##' @method as.function L_objective
##' @S3method as.function L_objective
as.function.L_objective <- function( x, ... ){
  L <- terms(x)[["L"]]
  out <- function(x)
    crossprod(L, x)
  class(out) <- c(class(out), class(x))
  out
}

##' Coerces objects of type \code{"L_objective"}.
##'
##' Objects from the following classes can be coerced to
##' \code{"L_objective"}: \code{"NULL"}, \code{"numeric"},
##' \code{"Q_objective"}, and \code{"function"}. The elements of a
##' \code{"numeric"} vector \eqn{c} are treated as being objective
##' variable coefficients in \eqn{c^\top x}). Coercing from
##' \code{"Q_objective"} simply removes the quadratic part from the
##' objective function. Coercing a \code{"function"} to
##' \code{"L_objective"} is only possible if the function also
##' inherits from class \code{"objective"}.
##' @title Linear Objective Functions
##' @param x an R object.
##' @return an object of class \code{"L_objective"} which inherits
##' from \code{"objective"}.
##' @author Stefan Theussl
##' @export
as.L_objective <- function( x )
    UseMethod( "as.L_objective" )

##' @nord
##' @method as.L_objective L_objective
##' @S3method as.L_objective L_objective
as.L_objective.L_objective <- identity

##' @nord
##' @method as.L_objective NULL
##' @S3method as.L_objective NULL
as.L_objective.NULL <- function( x )
    L_objective( x )

##' @nord
##' @method as.L_objective numeric
##' @S3method as.L_objective numeric
as.L_objective.numeric <- function( x )
    L_objective( x )

##' @nord
##' @method as.L_objective Q_objective
##' @S3method as.L_objective Q_objective
as.L_objective.Q_objective <- function( x )
    L_objective( terms(x)[["L"]])

##' @nord
##' @method as.L_objective function
##' @S3method as.L_objective function
as.L_objective.function <- function( x ){
    if( !inherits(x, "objective") )
        stop("'x' must be a function which inherits from 'objective'")
    L_objective( get("L", environment(x)) )
}



###############################################################
## quadratic objectives
###############################################################

##' A quadratic objective function is typically of the form
##' \eqn{x^\top Qx + c^\top x} where \eqn{Q} is a (sparse) matrix
##' defining the quadratic part of the function and \eqn{c} is a
##' (sparse) vector of coefficients to the \eqn{n} defining the linear
##' part.
##'
##' @title Quadratic Objective Function
##' @param Q a \eqn{n \times n} matrix with numeric entries representing the quadratic
##' part of objective function. Sparse matrices of class
##' \code{"simple_triplet_matrix"} can be supplied.
##' @param L a numeric vector of length \eqn{n}, where \eqn{n} is the
##' number of objective variables.
##' @author Stefan Theussl
##' @export
Q_objective <- function( Q, L = NULL ) {
    L <- as.L_term(L)
    if( !is.null(Q) )
        obj <- .objective( Q    = as.simple_triplet_matrix(0.5 * (Q + t(Q))),
                           L    = L,
                           nobj = dim(Q)[1] )
    else
        obj <- .objective( L = L, nobj = length(L) )
    class(obj) <- c( "Q_objective", class(obj) )
    obj
}

##' @nord
##' @method as.function Q_objective
##' @S3method as.function Q_objective
as.function.Q_objective <- function( x, ... ){
  L <- terms(x)[["L"]]
  ## FIXME: shouldn't this already be initialized earlier?
  if( !length(L) )
      L <- slam::simple_triplet_zero_matrix(ncol = length(x), nrow = 1L)

  Q <- terms(x)[["Q"]]
  out <- function(x)
      crossprod(L, x) + 0.5 * .xtQx(Q, x)
  class(out) <- c(class(out), class(x))
  out
}

##' Coerces objects of type \code{"Q_objective"}.
##'
##' Objects from the following classes can be coerced to
##' \code{"Q_objective"}: \code{"function"}, \code{"matrix"}, and
##' \code{"simple_triplet_matrix"}.
##' @title Quadratic Objective Function
##' @param x an R object.
##' @return an object of class \code{"Q_objective"} which inherits
##' from \code{"objective"}.
##' @author Stefan Theussl
##' @export
as.Q_objective <- function( x )
  UseMethod("as.Q_objective")

##' @nord
##' @method as.Q_objective function
##' @S3method as.Q_objective function
as.Q_objective.function <- function( x ){
  if( !inherits(x, "objective") )
    stop( "'x' must be a function which inherits from 'objective'" )
  L_objective( get("L", environment(x)) )
  Q_objective( L = get("L", environment(x)),
               Q = get("Q", environment(x)) )
}

##' @nord
##' @method as.Q_objective matrix
##' @S3method as.Q_objective matrix
as.Q_objective.matrix <- function( x )
  Q_objective( Q = x)

##' @nord
##' @method as.Q_objective numeric
##' @S3method as.Q_objective numeric
as.Q_objective.numeric <- function( x )
  Q_objective( Q = matrix(x))

##' @nord
##' @method as.Q_objective Q_objective
##' @S3method as.Q_objective Q_objective
as.Q_objective.Q_objective <- identity

##' @nord
##' @method as.Q_objective simple_triplet_matrix
##' @S3method as.Q_objective simple_triplet_matrix
as.Q_objective.simple_triplet_matrix <- function( x )
  Q_objective(Q = x)




###############################################################
## general objectives
###############################################################

## TODO:
F_objective <- function( F ) {
  structure ( list(F = as.function(F)),
             class = c("F_objective", "objective") )
}

as.function.F_objective <- function( x, ... )
  x$F

as.F_objective <- function(x, ...)
  UseMethod("as.F_objective")

as.F_objective.F_objective <- function(x, ... )
    identity( x )

as.F_objective.L_objective<- function( x, ... )
  F_objective( x )

as.F_objective.Q_objective<- function( x, ... )
  F_objective( x )

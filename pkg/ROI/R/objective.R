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

##  Extract the objective function from its argument (typically ROI
##  objects) and return them.
## 
##  The default method assumes that the supplied R object is a list
##  where the element \code{objective} represents the objective
##  function. The extractet element is then coerced to a function.
##' @title Objective - Accessor and Mutator Functions
##' @description The \link{objective} of a given optimization problem (\link{OP}) 
##'     can be accessed or mutated via the method \code{'objective'}.
##' @param x an object used to select the method.
##' @param value an R object.
##' @return a function inheriting from \code{"objective"}.
##' @author Stefan Theussl
##' @name objective (Set/Get)
##' @rdname objective
##' @examples
##' x <- OP()
##' objective(x) <- 1:3
##' @export
objective <- function( x )
    UseMethod( "objective" )

##' @noRd
##' @export
objective.default <- function( x )
    if (is.null(x$objective)) NULL else as.function( x$objective )

##' @rdname objective
##' @export objective<-
'objective<-' <- function( x, value )
    UseMethod("objective<-")


##' @noRd
##' @export
'objective<-.OP' <- function( x, value ) {
    obj <- as.objective(value)
    nvar <- length(obj)
    if ( is.na(x[["n_of_variables"]]) ) {
        x[["n_of_variables"]] <- nvar
    } else {
        stopifnot(isTRUE(x[["n_of_variables"]] == nvar))
    }
    x[["objective"]] <- obj
    x
}

## Coerces objects of type \code{"objective"}.
##
## @title Objective Function Utilities
## @param x an R object.
## @return an object of class \code{"objective"}.
## @author Stefan Theussl
##' @rdname objective
##' @export
as.objective <- function( x )
    UseMethod("as.objective")

##' @noRd
##' @export
as.objective.function <- function( x ){
    if( inherits(x, "Q_objective", which = TRUE) == 2 )
        return( as.Q_objective( x ) )
    if( inherits(x, "L_objective", which = TRUE) == 2 )
        return( as.L_objective( x ) )
    if( inherits(x, "F_objective", which = TRUE) == 2 )
        return( as.F_objective( x ) )
    stop("'x' must be of type L_objective, Q_objective or", 
         " F_objective, was ", shQuote(typeof(x)))
}

##' @noRd
##' @export
as.objective.default <- function( x ) as.L_objective( x )

##' @noRd
##' @export
as.objective.objective <- identity

##' @noRd
##' @export
length.objective <- function( x ) attr( as.objective(x), "nobj" )

##' @noRd
##' @export
`[.L_objective` <- function(x, i) {
    as.numeric(as.matrix(terms(x)$L[1, i]))
}

##  NOTE: Since we override the length of the objective the str function which 
##        relies on length, doesn't work anymore.
##' @noRd
##' @export
str.objective <- function(object, ...) {
    class(object) <- paste(shQuote(class(object)), collapse=" ")
    str(object)
}

##' @noRd
##' @export
terms.function <- function( x, ... ){
    if( inherits(x, "L_objective") )
        return( terms(as.L_objective(x)) )
    if( inherits(x, "Q_objective") )
        return( terms(as.Q_objective(x)) )
    if( inherits(x, "F_objective") )
        return( terms(as.F_objective(x)) )
    NA
}


###############################################################
## linear objectives
###############################################################

## Linear objective function (class 'L_objective')
## of type c^\top x, where c is a vector of coefficients

##' A linear objective function is typically of the form \deqn{c^\top
##' x} where \eqn{c} is a (sparse) vector of coefficients to the
##' \eqn{n} objective variables \eqn{x}.
##'
##' @title Linear Objective Function
##' @param L a numeric vector of length \eqn{n} or an object of class
##' \code{"simple_triplet_matrix"} (or coercible to such) with dimension \eqn{1 \times n},
##' where \eqn{n} is the number of objective variables. Names will be
##' preserved and used e.g., in the print method.
##' @param x an R object.
##' @param names an optional character vector giving the names of \eqn{x}
##'        (column names of \eqn{L}).
##' @return an object of class \code{"L_objective"} which inherits
##' from  \code{"Q_objective"} and \code{"objective"}.
##' @author Stefan Theussl
##' @export
L_objective <- function( L, names = NULL ) {
    if ( !is.null(names) ) {
        stopifnot( is.character(names), any(c(length(L), ncol(L)) == length(names)) )
    }
    obj <- Q_objective( Q = NULL, L = L, names = names )
    class( obj ) <- c( "L_objective", class(obj) )
    obj
}

##' @noRd
##' @export
as.function.L_objective <- function( x, ... ){
  L <- terms(x)[["L"]]
  names <- terms(x)[["names"]]
  out <- function(x)
      structure( c(slam::tcrossprod_simple_triplet_matrix(L, t(x))), names = rownames(L) )
  class(out) <- c(class(out), class(x))
  out
}

##' @rdname L_objective
##' @param ... further arguments passed to or from other methods
##' @export
terms.L_objective <- function( x, ... )
  list( L = x$L, names = x$names )


##  Coerces objects of type \code{"L_objective"}.
##
##  Objects from the following classes can be coerced to
##  \code{"L_objective"}: \code{"NULL"}, \code{"numeric"},
##  \code{"Q_objective"}, and \code{"function"}. The elements of a
##  \code{"numeric"} vector \eqn{c} are treated as being objective
##  variable coefficients in \eqn{c^\top x}). Coercing from
##  \code{"Q_objective"} simply removes the quadratic part from the
##  objective function. Coercing a \code{"function"} to
##  \code{"L_objective"} is only possible if the function also
##  inherits from class \code{"objective"}.
##  @title Linear Objective Functions
##  @param x an R object.
##  @return an object of class \code{"L_objective"} which inherits
##  from  \code{"Q_objective"} and \code{"objective"}.
##  @author Stefan Theussl
##' @rdname L_objective
##' @export
as.L_objective <- function( x )
    UseMethod( "as.L_objective" )

##' @noRd
##' @export
as.L_objective.L_objective <- identity

##' @noRd
##' @export
as.L_objective.NULL <- function( x )
    L_objective( x )

##' @noRd
##' @export
as.L_objective.numeric <- function( x )
    L_objective( x, names = names(x) )

##' @noRd
##' @export
as.L_objective.Q_objective <- function( x )
    L_objective( terms(x)[["L"]])

##' @noRd
##' @export
as.L_objective.function <- function( x ){
    if( !inherits(x, "objective") )
        stop("'x' must be a function which inherits from 'objective'")
    L_objective( get("L", environment(x)), get("names", environment(x)) )
}



###############################################################
## quadratic objectives
###############################################################

##' A quadratic objective function is typically of the form
##' \deqn{\frac{1}{2} x^\top Qx + c^\top x} where \eqn{Q} is a (sparse) matrix
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
##' @param names an optional character vector giving the names of \eqn{x}
##'        (row/column names of \eqn{Q}, column names of \eqn{L}).
##' @param x an R object.
##' @return an object of class \code{"Q_objective"} which inherits
##' from \code{"objective"}.
##' @author Stefan Theussl
##' @export
Q_objective <- function( Q, L = NULL, names = NULL ) {
    L <- as.L_term(L, nrow = 1L, ncol = ncol(Q))
    if( !is.null(Q) ) {
        stopifnot(nrow(Q) == ncol(Q))
        obj <- .objective( Q    = as.simple_triplet_matrix(0.5 * (Q + t(Q))),
                           L    = L, names = names,
                           nobj = dim(Q)[1])
    } else {
        obj <- .objective( L = L, names = names, nobj = ncol(L) )
    }
    class(obj) <- c( "Q_objective", class(obj) )
    obj
}

##' @noRd
##' @export
as.function.Q_objective <- function( x, ... ) {
    L <- terms(x)[["L"]]
    Q <- terms(x)[["Q"]]
    names <- terms(x)[["names"]]
    out <- function(x)
        structure( c(slam::tcrossprod_simple_triplet_matrix(L, t(x)) + 0.5 * .xtQx(Q, x)) )
    class(out) <- c(class(out), class(x))
    out
}

##' @rdname Q_objective
##' @param ... further arguments passed to or from other methods
##' @export
terms.Q_objective <- function( x, ... )
  list( Q = x$Q, L = x$L, names = x$names )

##  Coerces objects of type \code{"Q_objective"}.
##
##  Objects from the following classes can be coerced to
##  \code{"Q_objective"}: \code{"function"}, \code{"matrix"}, and
##  \code{"simple_triplet_matrix"}.
##  @title Quadratic Objective Function
##  @param x an R object.
##  @return an object of class \code{"Q_objective"} which inherits
##  from \code{"objective"}.
##  @author Stefan Theussl
##' @rdname Q_objective
##' @export
as.Q_objective <- function( x )
  UseMethod("as.Q_objective")

##' @noRd
##' @export
as.Q_objective.function <- function( x ){
  if( !inherits(x, "objective") )
    stop( "'x' must be a function which inherits from 'objective'" )
  Q_objective( L = get("L", environment(x)),
               Q = get("Q", environment(x)),
               names = get("names", environment(x)) )
}

##' @noRd
##' @export
as.Q_objective.matrix <- function( x )
  Q_objective( Q = x)

##' @noRd
##' @export
as.Q_objective.numeric <- function( x )
  Q_objective( Q = matrix(x))

##' @noRd
##' @export
as.Q_objective.Q_objective <- identity

##' @noRd
##' @export
as.Q_objective.simple_triplet_matrix <- function( x )
  Q_objective(Q = x)




###############################################################
## general objectives
###############################################################

##' General objective function \eqn{f(x)} to be optimized.
##'
##' @title General (Nonlinear) Objective Function
##' @param F an R \code{"function"} taking a numeric vector \code{x} of length \eqn{n} as argument.
##' @param G an R \code{"function"} returning the gradient at \code{x}.
##' @param H an optional \code{function} holding the Hessian of F.
##' @param n the number of objective variables.
##' @param names an optional character vector giving the names of x.
##' @return an object of class \code{"F_objective"} which inherits
##' from \code{"objective"}.
##' @author Stefan Theussl
##' @export
F_objective <- function( F, n, G = NULL, H = NULL, names=NULL ) {
    .check_function_for_sanity( F, n )
    ##if( !is.null(G) )
    ##    .check_gradient_for_sanity( G, n )
    obj <- .objective( F = F, G = G, H = H, 
                       names = names, nobj = as.integer(n) )
    class( obj ) <- c( "F_objective", class(obj) )
    obj
}

##' @noRd
##' @export
as.function.F_objective <- function( x, ... ){
    F <- x$F
    G <- x$G
    H <- x$H
    nobj <- attr(x, "nobj")
    names <- terms(x)[["names"]]
    out <- function(x) {
        F(x)
    }
    class(out) <- c(class(out), class(x))
    out
}

##' @rdname F_objective
##' @param x an R object.
##' @param ... further arguments passed to or from other methods
##' @export
terms.F_objective <- function( x, ... )
    list( F = x$F, n = x$n, G = x$G, H = x$H, names = x$names )

##  Coerces objects of type \code{"F_objective"}.
##
##  Objects from the following classes can be coerced to
##  \code{"F_objective"}: \code{"function"}, \code{"L_objective"}, and
##  \code{"Q_objective"}.
##  @title General Objective Function
##  @param x an R object.
##  @return an object of class \code{"F_objective"} which inherits
##  from \code{"objective"}.
##  @author Stefan Theussl
##' @rdname F_objective
##' @export
as.F_objective <- function( x )
  UseMethod("as.F_objective")

##' @noRd
##' @export
as.F_objective.F_objective <- function( x )
    return( x )

##' @noRd
##' @export
as.F_objective.L_objective <- function( x )
    F_objective( F = as.function(x), n = length(x), G = G(x), names = variable.names(x) )

##' @noRd
##' @export
as.F_objective.Q_objective <- function( x )
    F_objective( F = as.function(x), n = length(x), G = G(x), names = variable.names(x) )

##' @noRd
##' @export
as.F_objective.function <- function( x ){
    F <- get("F", environment(x))
    n <- get("nobj", environment(x))
    G <- get("G", environment(x))
    H <- get("H", environment(x))
    names <- get("names", environment(x))
    if( !inherits(x, "objective") )
        stop("'x' must be a function which inherits from 'objective'")
    F_objective( F = x, n = n, G = G, H = H, names = names )
}

.check_function_for_sanity <- function(F, n) {
    is_integer <- function(x) {
        if ( !is.numeric(x) )
            return(FALSE)
        (x - as.integer(x)) < .Machine$double.eps
    }
    stopifnot( is.function(F), is_integer(n) )
    ans <- tryCatch( F(rep.int(0, n)), error = identity )

    if( inherits(ans, "error") | (length(ans) != 1L) | !is.finite(ans) )
        stop(sprintf("cannot evaluate function 'F' using 'n' = %d parameters.", n))
    if( !is.numeric(ans) || (length(ans) != 1L) || !is.null(dim(ans)) )
        stop("function 'F' does not return a numeric vector of length 1.")
    invisible( ans )
}

##' @rdname L_objective
##' @param object an R object.
##' @export
variable.names.L_objective <- function(object, ...) {
    object$names
}

##' @rdname Q_objective
##' @param object an R object.
##' @export
variable.names.Q_objective <- function(object, ...) {
    object$names
}

##' @rdname F_objective
##' @param object an R object.
##' @export
variable.names.F_objective <- function(object, ...) {
    object$names
}

##' @noRd
##' @export
variable.names.function <- function(object, ...) {
    if ( inherits(object, "L_objective"))
        return(as.L_objective(object)$names)
    if ( inherits(object, "Q_objective"))
        return(as.Q_objective(object)$names)
    if ( inherits(object, "F_objective"))
        return(as.F_objective(object)$names)
    NULL
}

##' @noRd
##' @export
print.L_objective <- function(x, ...) {
    writeLines(sprintf("A linear objective of length %i.", length(x)))
}

##' @noRd
##' @export
print.Q_objective <- function(x, ...) {
    writeLines(sprintf("A quadratic objective of length %i.", length(x)))
}

##' @noRd
##' @export
print.F_objective <- function(x, ...) {
    writeLines(sprintf("A general objective function of length %i.", length(x)))
}

## <<< TODO: add a sanity check for the gradient! >>>
##.check_gradient_for_sanity <- function(F, n){
##    ans <- tryCatch( F(rep(n, 0)), error = identity )
##    if( inherits(ans, "error") )
##        stop(sprintf("cannot evaluate function 'F' using 'n' = %d parameters.", n))
##    if( !is.numeric(ans) || (length(ans) != n) || !is.null(dim(ans)) )
##        stop("function 'F' does not return a numeric vector of length 'n'.")
##    invisible( ans )
##}


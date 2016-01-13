################################################################################
## Package: ROI
## File:    constraints.R
## Author:  Stefan Theussl
## Changed: 2014-04-15
################################################################################

## NOTE: probably support "range" constraints in order to improve efficiency
## (lhs ~ f(x) ~ rhs)

################################################################################
## 'constraints' helper functions
################################################################################

available_constraint_classes <- function()
    c(L = "L_constraint", Q = "Q_constraint", F = "F_constraint", X = "NO_constraint")



################################################################################
## 'constraints' extractor functions
################################################################################

##' Extract constraints from its argument (typically ROI objects) and
##' return them.
##'
##' Currently, there is no default method. See \code{\link{constraints.OP}}
##' for extracting constraints from ROI objects of class \code{"OP"}.
##' @title Extract constraints
##' @param x an object used to select the method.
##' @return the extracted constraints object.
##' @author Stefan Theussl
##' @export
constraints <- function( x )
    UseMethod("constraints")

##' Extract constraints from ROI objects of class \code{"OP"} and
##' return them.
##'
##'
##' @title Extract constraints
##' @param x an object of class \code{"OP"}.
##' @return an object inheriting from class \code{"constraints"}.
##' @author Stefan Theussl
##' @export
constraints.OP <- function( x ){
    if( is.null(x$constraints) )
        NO_constraint( length(objective(x)) )
    else
        x$constraints
}



################################################################################
## 'constraints' replacement functions
################################################################################

##' Replaces the constraints in R objects (typically ROI
##' objects).
##'
##' Currently, there is no default method. Constraints in ROI objects
##' of class \code{"OP"} given by the argument \code{x} are replaced
##' with \code{value}, either being an object of class
##' \code{"constraint"}, coercible to such, or \code{NULL}
##' (unconstrained). The updated \code{"OP"} object will be returned.
##' @title Replacement of constraints
##' @name constraints-replace
##' @aliases constraints<- constraints<-.OP
##' @usage constraints(x) <- value
##' @param x an R object.
##' @param value an R object.
##' @return the updated object.
##' @author Stefan Theussl
##' @export constraints<-
'constraints<-' <- function( x, value )
    UseMethod("constraints<-")

##' @noRd
##' @export
'constraints<-.OP' <- function( x, value ) {
    x$constraints <- as.constraint(value)
    x
}



################################################################################
## No constraints (class 'NO_constraint')
## 0xn simple_triplet_zero_matrix
################################################################################

##' In case the \code{constraints} slot in the problem object is
##' \code{NULL} the return value of a call of \code{constraints()}
##' will return an object of class \code{"NO_constraint"} which
##' inherits from \code{"L_constraint"}.
##'
##' @title Class: \code{"NO_constraint"}
##' @param n_obj a numeric vector of length \code{1} representing the number
##' of objective variables.
##' @return an object of class \code{"NO_constraint"} which inherits
##' from \code{"L_constraint"} and \code{"constraint"}.
##' @author Stefan Theussl
##' @import slam
##' @export
NO_constraint <- function( n_obj ) {
    n_obj <- as.integer( n_obj )
    stopifnot( length(n_obj) == 1L )
    structure( list(),
               n_constraints = 0,
               n_obj = n_obj,
               class = c("NO_constraint", "constraint") )
}

##' Coerces objects of type \code{"NO_constraint"}.
##'
##' (Almost) all objects inheriting from class \code{"constraint"} can be
##' coerced to \code{"NO_constraint"}. It extracts the number of
##' (objective) variables from the original constraints objects and
##' returns the \code{"NO_constraint"} object.
##' @title Coercing Constraints
##' @param x an R object.
##' @param ... further arguments passed to or from other methods
##' (currently ignored).
##' @return an object of class \code{"NO_constraint"} which inherits
##' from \code{"constraint"}.
##' @author Stefan Theussl
##' @export
as.NO_constraint <- function(x, ...)
    UseMethod( "as.NO_constraint" )

##' @noRd
##' @export
as.NO_constraint.NO_constraint <- function( x, ... )
    x

##' @noRd
##' @export
as.NO_constraint.L_constraint <- function( x, ... )
    NO_constraint( ncol(x$L) )

##' Tests if an object is interpretable as being of class \code{"L_constraint"}.
##'
##' @title Class: \code{"NO_constraint"}
##' @param x object to be tested.
##' @return returns \code{TRUE} if its argument is of class
##' \code{"NO_constraint"} and \code{FALSE} otherwise.
##' @author Stefan Theussl
##' @export
is.NO_constraint <- function( x ) {
    inherits( x, "NO_constraint" )
}

##' Take a sequence of constraints (ROI objects) arguments and combine
##' by rows, i.e., putting several constraints together.
##'
##' The output type is determined from the highest type of the
##' components in the hierarchy NULL < \code{"NO_constraint"} <
##' \code{"L_constraint"} < \code{"Q_constraint"} <
##' \code{"F_constraint"}.
##'
##' @title Class: \code{"NO_constraint"}
##' @param ... constraints objects to be concatenated.
##' @param recursive logical. Currently ignored (enable compatibility
##' with \code{c()} operator).
##' @return an object of a class depending on the input which also
##' inherits from \code{"constraint"}. See \bold{Details}.
##' @author Stefan Theussl
##' @export
rbind.NO_constraint <- function( ..., recursive = FALSE ){
    constraints <- list(...)
    nc <- unlist(lapply(constraints, is.NO_constraint))
    dims <- unlist(lapply(constraints, function(x) dim(x)[1]))
    stopifnot( all(dims == dims[1]) )
    ## FIXME: shouldn't we check dimensionaliy of constraints?
    if( all(nc) )
        constraints[[1]]
    else
        stop( "can only rbind objects of class 'NO_constraint'." )
}

## @export
## rbind.constraint <- function( ..., recursive = FALSE ){
##     rbind.NO_constraint( ..., recursive = recursive )
##}

##' @noRd
##' @export
c.NO_constraint <- function( ..., recursive = FALSE )
    rbind( ..., recursive = recursive )

##' Get the number of constraints from a corresponding ROI object.
##'
##' @title Class: \code{"NO_constraint"}
##' @param x constraints object.
##' @return an integer.
##' @author Stefan Theussl
##' @export
length.NO_constraint <- function( x )
    attr(x, "n_constraints")



################################################################################
## Linear constraints (class 'L_constraint')
## Ax ~ b
################################################################################

##' Linear constraints are typically of the form \eqn{Ax \leq
##' b}. \eqn{A} is a (sparse) matrix of coefficients to the objective
##' variables \eqn{x}. \eqn{b} is called the right hand side of the
##' constraints.
##'
##' @title Linear Constraints
##' @param L a numeric vector of length \eqn{n} (a single constraint)
##' or a matrix of dimension \eqn{n \times m}, where \eqn{n} is the
##' number of objective variables and \eqn{m} is the number of
##' constraints. Matrices can be of class
##' \code{"simple_triplet_matrix"} to allow a sparse representation of
##' constraints.
##' @param dir a character vector with the directions of the
##' constraints. Each element must be one of \code{"<"}, \code{"<="},
##' \code{">"}, \code{">="}, \code{"=="} or \code{"!="}.
##' @param rhs a numeric vector with the right hand side of the constraints.
##' @return an object of class \code{"L_constraint"} which inherits
##' from \code{"constraint"}.
##' @author Stefan Theussl
##' @export
L_constraint <- function( L, dir, rhs ) {
    L     <- as.L_term(L)
    stopifnot( row_sense_is_feasible(dir) )
    rhs   <- as.rhs( rhs )
    dim_L <- dim( L )
    n_dir <- length( dir )
    n_L_constraints <- length( rhs )
    stopifnot( all(c(dim_L[ 1 ], n_dir) == n_L_constraints) )
    ## FIXME: we cannot use this check since we need zero row
    ##L_constraints for coercing NO_constraonts to L_constraints
    ##stopifnot( n_L_constraints >= 1 )
    structure( list(L   = L,
                    dir = dir,
                    rhs = rhs),
              n_L_constraints = n_L_constraints,
              class = c("L_constraint", "Q_constraint", "constraint") )
}

##' Coerces objects of type \code{"L_constraint"}.
##'
##' Objects from the following classes can be coerced to
##' \code{"L_constraint"}: \code{"numeric"} and \code{"list"}. The
##' elements of a \code{"numeric"} vector \eqn{a} are treated as
##' objective variable coefficients of a single constraint in standard
##' form (\eqn{ax \geq 0}). A \code{"list"} must contain three
##' elements, the matrix \eqn{A}, the direction of constraints, and
##' the right hand side defining a linear constraint \eqn{Ax \leq b}.
##' @title Linear Constraints
##' @param x an R object.
##' @param ... further arguments passed to or from other methods
##' (currently ignored).
##' @return an object of class \code{"L_constraint"} which inherits
##' from \code{"constraint"}.
##' @author Stefan Theussl
##' @export
as.L_constraint <- function(x, ...)
    UseMethod("as.L_constraint")


##' @noRd
##' @export
as.L_constraint.L_constraint <- function( x, ... )
    identity(x)

##' @noRd
##' @export
as.L_constraint.numeric <- function( x, ... )
    L_constraint( L = x, dir = ">=", rhs = 0 )

##' @noRd
##' @export
as.L_constraint.list <- function( x, ... ){
    names(x) <- c("L", "dir", "rhs")
    L_constraint( L = x$L, dir = x$dir, rhs = x$rhs )
}

##' @noRd
##' @export
as.L_constraint.NO_constraint<- function( x, ... )
    L_constraint( L = simple_triplet_zero_matrix(nrow = length(x), ncol = dim(x)[2]),
                  dir = NULL, rhs = NULL )

##' Tests if an object is interpretable as being of class \code{"L_constraint"}.
##'
##' @title Linear Constraints
##' @param x object to be tested.
##' @return returns \code{TRUE} if its argument is of class
##' \code{"L_constraint"} and \code{FALSE} otherwise.
##' @author Stefan Theussl
##' @export
is.L_constraint <- function( x ) {
    inherits( x, "L_constraint" )
}

## combining matrices (see 'rbind' in matrix.R, package relation)

##' Take a sequence of constraints (ROI objects) arguments and combine
##' by rows, i.e., putting several constraints together.
##'
##' The output type is determined from the highest type of the
##' components in the hierarchy NULL < \code{"L_constraint"} <
##' \code{"Q_constraint"} < \code{"F_constraint"}.
##'
##' @title Linear Constraints
##' @param ... constraints objects to be concatenated.
##' @param recursive logical. Currently ignored (enable compatibility
##' with \code{c()} operator).
##' @return an object of a class depending on the input which also
##' inherits from \code{"constraint"}. See \bold{Details}.
##' @author Stefan Theussl
##' @export
rbind.L_constraint <- function( ..., recursive = FALSE ){
    constraints <- lapply(list(...), as.L_constraint)
    L   <- lapply( constraints, function (x) as.simple_triplet_matrix(x$L) )
    dir <- lapply( constraints, function (x) as.character(x$dir) )
    rhs <- lapply( constraints, function (x) as.rhs(x$rhs) )
    L_constraint( L =   Reduce(function(x, y) rbind(x, y), L),
                 dir = Reduce(function(x, y) c(x, y), dir),
                 rhs = Reduce(function(x, y) c(x, y), rhs) )
}

## FIXME: connection to rbind documentation

##' @noRd
##' @export
c.L_constraint <- function( ..., recursive = FALSE )
    rbind( ..., recursive = recursive )

##' Get the number of constraints from a corresponding ROI object.
##'
##' @title Linear Constraints
##' @param x constraints object.
##' @return an integer.
##' @author Stefan Theussl
##' @export
length.L_constraint <- function( x )
    attr(x, "n_L_constraints")
## the linear term of the left hand side

as.L_term <- function( x, ... )
    UseMethod("as.L_term")

##' @noRd
##' @export
as.L_term.numeric <- function( x, ... )
    as.simple_triplet_matrix( matrix(x, nrow = 1L) )

##' @noRd
##' @export
as.L_term.matrix <- function( x, ... )
    as.simple_triplet_matrix(x)

##' @noRd
##' @export
as.L_term.simple_triplet_matrix <- function( x, ... )
    x

##' @noRd
##' @export
as.L_term.NULL <- function( x, ... )
    x


################################################################################
## Quadratic constraints (class 'Q_constraint')
## list of constraints of the form a'x + x'Qx ~ b
################################################################################

##' Quadratic constraints are typically of the form
##' \eqn{\frac{1}{2}x^{\top}Qx + c^{\top}x \leq b}. \eqn{A} is a
##' (sparse) matrix of coefficients to the objective variables \eqn{x}
##' of the quadratic part and \eqn{c} is the vector of coefficients of
##' the linear part of a given constraint. \eqn{b} is called the right
##' hand side of the constraints.
##'
##' @title Quadratic Constraints
##' @param Q a list of (sparse) matrices representing the quadratic
##' part of each constraint.
##' @param L a numeric vector of length \eqn{n} (a single constraint)
##' or a matrix of dimension \eqn{n \times m}, where \eqn{n} is the
##' number of objective variables and \eqn{m} is the number of
##' constraints. Matrices can be of class
##' \code{"simple_triplet_matrix"} to allow a sparse representation of
##' constraints.
##' @param dir a character vector with the directions of the
##' constraints. Each element must be one of \code{"<"}, \code{"<="},
##' \code{">"}, \code{">="}, \code{"=="} or \code{"!="}.
##' @param rhs a numeric vector with the right hand side of the
##' constraints.
##' @return an object of class \code{"Q_constraint"} which inherits
##' from \code{"constraint"}.
##' @author Stefan Theussl
##' @export
Q_constraint <- function(Q, L, dir, rhs){
    Q     <- as.Q_term( Q )
    L     <- as.L_term( L )
    stopifnot( row_sense_is_feasible(dir) )
    rhs   <- as.rhs( rhs )
    dim_L <- dim( L )
    n_Q   <- length( Q )
    dim_Q <- lapply( Q, dim )
    n_dir <- length( dir )
    n_Q_constraints <- length( rhs )
    ## all Q need to be nxn and L kxn
    stopifnot( all(unlist(dim_Q) == dim_L[ 2 ]) )
    ## length of dir and rhs, as well as rows of L need to be equal
    stopifnot( all(c(dim_L[ 1 ], n_dir) == n_Q_constraints) )
    structure( list(Q   = Q,
                    L   = L,
                    dir = dir,
                    rhs = rhs),
               n_Q_constraints = n_Q_constraints,
               class = c("Q_constraint", "constraint") )
}

##' Coerces objects of type \code{"Q_constraint"}.
##'
##' Objects from the following classes can be coerced to
##' \code{"Q_constraint"}: \code{"list"}. The \code{"list"} must
##' contain four elements, a list of matrices \eqn{Q_m} representing
##' the quadratic part of \eqn{m} constraints, the matrix \eqn{A}
##' describing the linear part, the direction of the constraints, and
##' the right hand side.
##' @title Quadratic Constraints
##' @param x an R object.
##' @param ... further arguments passed to or from other methods
##' (currently ignored).
##' @return an object of class \code{"Q_constraint"} which inherits
##' from \code{"constraint"}.
##' @author Stefan Theussl
##' @export
as.Q_constraint <- function( x )
    UseMethod("as.Q_constraint")

##' @noRd
##' @export
as.Q_constraint.Q_constraint <-
    identity

##' @noRd
##' @export
as.Q_constraint.list <- function( x ){
    names(x) <- c("Q", "L", "dir", "rhs")
    Q_constraint( Q = x$Q, L = x$L, dir = x$dir, rhs = x$rhs )
}

##' Tests if an object is interpretable as being of class \code{"Q_constraint"}.
##'
##' @title Quadratic Constraints
##' @param x object to be tested.
##' @return returns \code{TRUE} if its argument is of class
##' \code{"Q_constraint"} and \code{FALSE} otherwise.
##' @author Stefan Theussl
##' @export
is.Q_constraint <- function( x ) {
    inherits( x, "Q_constraint" )
}

## TODO: Q part not implemented
rbind.Q_constraint <- function( ..., recursive = FALSE ){
    constraints <- lapply(list(...), as.Q_constraint)

    warning("Q part not implemented.")
    L_constraint( L =   Reduce(function(x, y) rbind(x, y), lapply( constraints, function (x) as.simple_triplet_matrix(x$L) )),
                 dir = Reduce(function(x, y) c(x, y), lapply( constraints, function (x) as.character(x$dir) )),
                 rhs = Reduce(function(x, y) c(x, y), lapply( constraints, function (x) as.rhs(x$rhs) )) )
}

c.Q_constraint <- function( ..., recursive = FALSE )
    rbind( ..., recursive = recursive )

length.Q_constraint <- function(x)
    attr(x, "n_Q_constraints")

## the quadratic term of the left hand side

as.Q_term <- function(x, ...)
    UseMethod( "as.Q_term" )

##' @noRd
##' @export
as.Q_term.list <- function( x )
    lapply( x, function(x) if( !is.null(x) ) as.simple_triplet_matrix(x) )

##' @noRd
##' @export
as.Q_term.numeric <- function( x )
    list( as.simple_triplet_matrix( matrix(x)) )

##' @noRd
##' @export
as.Q_term.matrix <- function( x )
    list( as.simple_triplet_matrix(x) )

##' @noRd
##' @export
as.Q_term.simple_triplet_matrix <- function( x )
    list( x )

## combine, print, and summary methods

##summary.Q_constraint <- function(x){
##
##}


## FIXME: Function constraints still incomplete and untested

################################################################################
## Function constraints (class 'F_constraint')
## list of constraints of the form f(x) ~ b
################################################################################

##' Function (or generally speaking nonlinear) constraints are
##' typically of the form \eqn{f(x) \leq b}. \eqn{f()} is a
##' well-defined R function taking the objective variables \eqn{x}
##' (typically a numeric vector) as arguments. \eqn{b} is called the
##' right hand side of the constraints.
##'
##' @title Function Constraints
##' @param F a \code{function} or a list of \code{function}s of length
##' \eqn{m}. Each \code{function} takes \eqn{n} parameters as input
##' and must return a skalar. Thus, \eqn{n} is the number of objective
##' variables and \eqn{m} is the number of constraints.
##' @param dir a character vector with the directions of the
##' constraints. Each element must be one of \code{"<"}, \code{"<="},
##' \code{">"}, \code{">="}, \code{"=="} or \code{"!="}.
##' @param rhs a numeric vector with the right hand side of the constraints.
##' @return an object of class \code{"F_constraint"} which inherits
##' from \code{"constraint"}.
##' @author Stefan Theussl
##' @export
F_constraint <- function(F, dir, rhs){
    F     <- as.F_term( F )
    stopifnot( row_sense_is_feasible(dir) )
    rhs   <- as.rhs( rhs )
    n_F   <- length( F )
    n_dir <- length( dir )
    n_F_constraints <- length( rhs )
    ## length of F, dir and rhs need to be equal
    stopifnot( all(c(n_F, n_dir) == n_F_constraints) )
    structure( list(F   = F,
                    dir = dir,
                    rhs = rhs),
              n_F_constraints = n_F_constraints,
              class = c("F_constraint", "constraint"))
}

## FIXME: there are still F_constraint methods to implement
as.F_constraint <- function(x, ...)
    UseMethod( "as.F_constraint")

as.F_term <- function(x, ...)
    UseMethod( "as.F_term" )

length.F_constraint <- function(x)
    attr( x, "n_F_constraints" )

as.F_term.function <- function(x)
    list( x )

as.F_term.list <- function(x)
    lapply( x, as.function )


################################################################################
## constraint helper functions

as.rhs <- function( x )
    UseMethod("as.rhs")

##' @noRd
##' @export
as.rhs.numeric <- identity

##' @noRd
##' @export
as.rhs.NULL <- function( x )
    numeric(0)

##' Coerces objects of type \code{"constraint"}.
##'
##' @title Constraint Utilities
##' @param x an R object.
##' @return an object inheriting from \code{"constraint"}.
##' @author Stefan Theussl
##' @export
as.constraint <- function( x )
    UseMethod("as.constraint")

##' @noRd
##' @export
as.constraint.NULL <- identity


##' @noRd
##' @export
as.constraint.L_constraint <-
    identity

##' @noRd
##' @export
as.constraint.Q_constraint <-
    identity

##' @noRd
##' @export
as.constraint.F_constraint <-
    identity

##' @noRd
##' @export
as.constraint.numeric <- function( x )
    as.L_constraint( x )

##' @noRd
##' @export
print.constraint <- function( x, ... ){
    len <- length(x)
    if( is.L_constraint(x) )
        writeLines( sprintf("An object containing %d linear constraints.", len) )
    else
        if( is.Q_constraint(x) )
            writeLines( c(sprintf("An object containing %d constraints.", len),
                          "Some constraints are of type quadratic.") )
        else
            writeLines( c(sprintf("An object containing %d constraints.", len),
                          "Some constraints are of type nonlinear.") )

    invisible(x)
}

##' @noRd
##' @export
dim.constraint <- function( x ){
    ## FIXME: we should actually save both dimensions in constraint object
    out <- if( inherits(x, "NO_constraint") )
        c( length(x), attributes(x)$n_obj )
    else if( inherits(x, "L_constraint") )
        c( length(x), ncol(x$L))
    else if( inherits(x, "Q_constraint") )
        c( length(x), unique(unlist(lapply( x$Q, dim ))) )
    else if( inherits(x, "F_constraint") ){
        warning( "Not implemented." )
        NULL
    }
    out
}


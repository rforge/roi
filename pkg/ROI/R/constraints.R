################################################################################
## Package: ROI
## File:    constraints.R
## Author:  Stefan Theussl
## Changed: 2016-05-20
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
##' @param value an R object.
##' @return the extracted constraints object.
##' @author Stefan Theussl
##' @export
constraints <- function( x )
    UseMethod("constraints")

##  Extract constraints from ROI objects of class \code{"OP"} and
##  return them.
## 
## 
##  @title Extract constraints
##  @param x an object of class \code{"OP"}.
##  @return an object inheriting from class \code{"constraints"}.
##  @author Stefan Theussl
##' @rdname constraints
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

##  Replaces the constraints in R objects (typically ROI
##  objects).
## 
##  Currently, there is no default method. Constraints in ROI objects
##  of class \code{"OP"} given by the argument \code{x} are replaced
##  with \code{value}, either being an object of class
##  \code{"constraint"}, coercible to such, or \code{NULL}
##  (unconstrained). The updated \code{"OP"} object will be returned.
##  @title Replacement of constraints
##  @name constraints-replace
##  @aliases constraints<- constraints<-.OP
##  @usage constraints(x) <- value
##  @param x an R object.
##  @param value an R object.
##  @return the updated object.
##  @author Stefan Theussl
##' @rdname constraints
##' @export constraints<-
'constraints<-' <- function( x, value )
    UseMethod("constraints<-")

##' @noRd
##' @export
'constraints<-.OP' <- function( x, value ) {
    x$constraints <- as.constraint(value)
    x
}


## combining matrices (see 'rbind' in matrix.R, package relation)

##' Take a sequence of constraints (ROI objects) arguments and combine
##' by rows, i.e., putting several constraints together.
##'
##' The output type is determined from the highest type of the
##' components in the hierarchy \code{"L_constraint"} <
##' \code{"Q_constraint"} < \code{"F_constraint"}.
##'
##' @title Linear Constraints
##' @param ... constraints objects to be concatenated.
##' @param use.names a logical if \code{FALSE} the names of the constraints
##'   are ignored when combining them, if \code{TRUE} the constraints are
##'   combined based on their \code{variable.names}.
##' @param recursive logical. Currently ignored (enable compatibility
##' with \code{c()} operator).
##' @return an object of a class depending on the input which also
##' inherits from \code{"constraint"}. See \bold{Details}.
##' @author Stefan Theussl
##' @export
rbind.constraint <- function(..., use.names=FALSE, recursive=FALSE) {
    constraints <- list(...)
    if (recursive) {
        constraints <- flatten_constraints(constraints, "", "rbind.constraint")
    }
    is_constraint <- sapply(constraints, inherits, what="constraint")
    if ( !all(is_constraint) ) {
        sc <- as.character(sys.call())[which(!is_constraint) + 1L]
        msg <- sprintf("object %s doesn't inherit from constraint!",
                       paste(sc, collapse=", "))
        error("TYPE_MISMATCH", msg, domain = "rbind.constraint", call=sys.call())
    }
    constraint_types <- sapply(constraints, function(x) class(x)[1])
    if ( length( constraints ) == 1L ) return( constraints[[1L]] )
    if ( "F_constraint" %in% constraint_types ) {
        return( rbind_F_constraint(constraints) )
    } else if ( "Q_constraint" %in% constraint_types ) {
        return( rbind_Q_constraint(constraints, use.names=use.names) )
    } else if ( "L_constraint" %in% constraint_types ) {
        return( rbind_L_constraint(constraints, use.names=use.names) )
    }
    return( rbind_NO_constraint(constraints) )
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
##' @param x an R object.
##' @param ... further arguments passed to or from other methods
##' (currently ignored).
##' @return an object of class \code{"NO_constraint"} which inherits
##' from \code{"L_constraint"} and \code{"constraint"}.
##' @author Stefan Theussl
##' @export
NO_constraint <- function( n_obj ) {
    n_obj <- as.integer( n_obj )
    stopifnot( length(n_obj) == 1L )
    structure( list(),
               n_constraints = 0,
               n_obj = n_obj,
               class = c("NO_constraint", "constraint") )
}

##  Coerces objects of type \code{"NO_constraint"}.
## 
##  (Almost) all objects inheriting from class \code{"constraint"} can be
##  coerced to \code{"NO_constraint"}. It extracts the number of
##  (objective) variables from the original constraints objects and
##  returns the \code{"NO_constraint"} object.
##  @title Coercing Constraints
##  @param x an R object.
##  @param ... further arguments passed to or from other methods
##  (currently ignored).
##  @return an object of class \code{"NO_constraint"} which inherits
##  from \code{"constraint"}.
##  @author Stefan Theussl
##' @rdname NO_constraint
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

##  Tests if an object is interpretable as being of class \code{"L_constraint"}.
## 
##  @title Class: \code{"NO_constraint"}
##  @param x object to be tested.
##  @return returns \code{TRUE} if its argument is of class
##  \code{"NO_constraint"} and \code{FALSE} otherwise.
##  @author Stefan Theussl
##' @rdname NO_constraint
##' @export
is.NO_constraint <- function( x ) {
    inherits( x, "NO_constraint" )
}

rbind_NO_constraint <- function( ..., recursive = FALSE ){
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

##' @noRd
##' @export
c.NO_constraint <- function( ..., recursive = FALSE )
    rbind( ..., recursive = recursive )

##  Get the number of constraints from a corresponding ROI object.
## 
##  @title Class: \code{"NO_constraint"}
##  @param x constraints object.
##  @return an integer.
##  @author Stefan Theussl
##  @rdname NO_constraint
##' @export
length.NO_constraint <- function( x )
    attr(x, "n_constraints")


################################################################################
## Linear constraints (class 'L_constraint')
## Ax ~ b
################################################################################

##' @noRd
##' @export
variable.names.Q_constraint <- function(object, ...) {
    object$names
}

##' @noRd
##' @export
variable.names.L_constraint <- function(object, ...) {
    object$names
}

##' Linear constraints are typically of the form \eqn{Ax \leq b}.
##' \eqn{A} is a (sparse) matrix of coefficients to the objective
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
##' @param names an optional character vector giving the names of the \eqn{A} matrix.
##' @param x an R object.
##' @param ... further arguments passed to or from other methods
##' (currently ignored).
##' @return an object of class \code{"L_constraint"} which inherits
##' from \code{"constraint"}.
##' @author Stefan Theussl
##' @export
L_constraint <- function( L, dir, rhs, names = NULL ) {
    L     <- as.L_term(L)
    stopifnot( row_sense_is_feasible(dir) )
    rhs   <- as.rhs( rhs )
    dim_L <- dim( L )
    n_dir <- length( dir )
    n_L_constraints <- length( rhs )
    if ( (!is.null(names)) & (length(names) != ncol(L)) ) {
        stop("number of columns of 'L' and length 'names' must be equal.")
    }
    stopifnot( all(c(dim_L[ 1 ], n_dir) == n_L_constraints) )
    structure( list(L   = L,
                    dir = dir,
                    rhs = rhs,
                    names = names),
              n_L_constraints = n_L_constraints,
              class = c("L_constraint", "Q_constraint", "constraint") )
}

## Coerces objects of type \code{"L_constraint"}.
##
## Objects from the following classes can be coerced to
## \code{"L_constraint"}: \code{"numeric"} and \code{"list"}. The
## elements of a \code{"numeric"} vector \eqn{a} are treated as
## objective variable coefficients of a single constraint in standard
## form (\eqn{ax \geq 0}). A \code{"list"} must contain three
## elements, the matrix \eqn{A}, the direction of constraints, and
## the right hand side defining a linear constraint \eqn{Ax \leq b}.
## @title Linear Constraints
## @param x an R object.
## @param ... further arguments passed to or from other methods
## (currently ignored).
## @return an object of class \code{"L_constraint"} which inherits
## from \code{"constraint"}.
## @author Stefan Theussl
##' @rdname L_constraint
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

## Tests if an object is interpretable as being of class \code{"L_constraint"}.
##
## @title Linear Constraints
## @param x object to be tested.
## @return returns \code{TRUE} if its argument is of class
## \code{"L_constraint"} and \code{FALSE} otherwise.
## @author Stefan Theussl
##' @rdname L_constraint
##' @export
is.L_constraint <- function( x ) {
    inherits( x, "L_constraint" )
}

## rbind_stm_by_names is a utility function to 'rbind' two 'simple_tiplet_matrices'
## into one based on provided columns names. Thereby it has to be ensured
## that the column names of b are a subset from the columns names of a.
rbind_stm_by_names <- function(a, b, a_names, b_names) {
    m <- match(b_names, a_names)
    a$dimnames <- NULL
    b$dimnames <- NULL
    b$ncol <- ncol(a)
    tmp <- b$j
    for (i in seq_along(m)) {
        b$j[tmp == i] <- m[i]
    }
    rbind(a, b)
}

## fill_stm is a utility function to fill the the Q matrix ( t(x) %*% Q %*% x )
## with zeros based on the 'new_names' provided. Thereby is assumed that 'old_names'
## is a subset of new_names.
fill_stm <- function(stm, old_names, new_names) {
    m <- match(old_names, new_names)
    stm$nrow <- length(new_names)
    stm$ncol <- length(new_names)
    for (i in seq_along(m)) {
        stm$i[stm$i == i] <- m[i]
        stm$j[stm$j == i] <- m[i]
    }
    return(stm)
}

## combine 2 L_constraints (ignore names)
c2_L_constraints <- function(x, y) {
    L_constraint( L=rbind(x$L, y$L), dir=c(x$dir, y$dir), rhs=c(x$rhs, y$rhs),
                  names=x$names )
}

## combine 2 L_constraints (use names, using the names allows to 'rbind' 2 L_constraints
## where the number of columns to not match by matching the names)
c2_L_constraints_named <- function(x, y) {
    L_constraint( L=rbind_stm_by_names(x$L, y$L, variable.names(x), variable.names(y)),
                  dir=c(x$dir, y$dir), rhs=c(x$rhs, y$rhs), names=variable.names(x) )
}

## combine 2 Q_constraints (ignore names)
c2_Q_constraints <- function(x, y) {
    Q_constraint(Q=c(x$Q, y$Q), L=rbind(x$L, y$L), dir=c(x$dir, y$dir),
                 rhs=c(x$rhs, y$rhs), names=x$names)
}

rbind_L_constraint <- function( constraints, use.names = FALSE) {
    constraints <- lapply(constraints, as.L_constraint)
    if ( length(constraints) == 1L ) return( constraints[[1L]] )
    if ( use.names ) {
        var.names <- lapply(constraints, function(x) variable.names(x))
        if ( any(sapply(var.names, is.null)) ) {
            stop("all constraints need to be named if 'use.names' is TRUE!")
        }
        is_equal <- function(x, y) isTRUE(all.equal(x, y))
        if ( all(mapply(is_equal, var.names[-1L], var.names[-length(var.names)])) ) {
            return( Reduce(c2_L_constraints, constraints) )
        } else {
            var.names <- unique(unlist(var.names))
            lc <- L_constraint(simple_triplet_zero_matrix(0L, length(var.names)),
                               NULL, NULL, names=var.names)
            return( Reduce(c2_L_constraints_named, c(list(lc), constraints)) )
        }
    } else {
        return( Reduce(c2_L_constraints, constraints) )
    }
}

## FIXME: connection to rbind documentation

##' @noRd
##' @export
c.L_constraint <- function( ..., recursive = FALSE )
    rbind( ..., recursive = recursive )

##  Get the number of constraints from a corresponding ROI object.
## 
##  @title Linear Constraints
##  @param x constraints object.
##  @return an integer.
##  @author Stefan Theussl
##' @rdname L_constraint
##' @export
length.L_constraint <- function( x )
    attr(x, "n_L_constraints")

##  the linear term of the left hand side
##  ----------------------------------------------------------
##  as.L_term
##  =========
##' @title Coerce an object to type \code{"L_term"}.
##' @description
##'   The \code{"L_term"} object represents the linear term of the \code{"L_constraint"}.
##'   Objects from the following classes can be coerced to \code{"L_term"}:
##'   \code{"NULL"}, \code{"numeric"}, \code{"matrix"}, \code{"simple_triplet_matrix"}
##'   and \code{"list"}.
##' @details
##'   In the case of \code{lists} \code{"as.Q_term"} is applied to every element
##'   of the list, for \code{NULL} one can supply the optional arguments \code{"nrow"}
##'   and \code{"ncol"} which will create a \code{"simple_triplet_zero_matrix"}
##'   with the specified dimension.
##' @param x an R object.
##' @param ... further arguments passed to or from other methods.
##' @return an object of class \code{"simple_triplet_zero_matrix"}
##' @export
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
as.L_term.NULL <- function( x, ... ) {
    dims <- list(...)
    if ( all(c("nrow", "ncol") %in% names(dims)) ) {
        return( simple_triplet_zero_matrix(dims$nrow, dims$ncol) )
    }
    x
}

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
##' @param names an optional character vector giving the names of the \eqn{A} matrix.
##' @param x an R object.
##' @param ... further arguments passed to or from other methods
##' (currently ignored).
##' @return an object of class \code{"Q_constraint"} which inherits
##' from \code{"constraint"}.
##' @author Stefan Theussl
##' @export
Q_constraint <- function(Q, L, dir, rhs, names=NULL) {
    L_is_vec <- is.vector(L)
    Q <- as.Q_term( Q, nrow=NROW(L), ncol=NCOL(L) )
    L <- as.L_term( L, nrow=length(Q), ncol=NROW(Q[[1L]]) )
    stopifnot( row_sense_is_feasible(dir) )
    rhs   <- as.rhs( rhs )
    dim_L <- dim( L )
    n_Q   <- length( Q )
    dim_Q <- lapply( Q, dim )
    n_dir <- length( dir )
    n_Q_constraints <- length( rhs )
    ## all Q need to be nxn and L kxn
    if( any(unlist(dim_Q) != dim_L[ 2 ]) ) {
        hint <- if (L_is_vec) "If 'L' is a vector it is converted to a column matrix." else NULL
        error("DIMENSION_MISMATCH", "The dimensions of 'Q' and 'L' don't match!",
              domain = "Q_constraint", hint = hint,
              note = c("The length of the list 'Q' is required to be equal to",
                       "the number of rows of the matrix 'L'!"))
    }
    ## length of dir and rhs, as well as rows of L need to be equal
    stopifnot( all(c(dim_L[ 1 ], n_dir) == n_Q_constraints) )
    if ( (!is.null(names)) & (length(names) != NCOL(L)) ) {
        stop("number of columns of 'Q' and length 'names' must be equal.")
    }
    structure( list(Q   = Q,
                    L   = L,
                    dir = dir,
                    rhs = rhs,
                    names = names),
               n_Q_constraints = n_Q_constraints,
               class = c("Q_constraint", "constraint") )
}

##  Coerces objects of type \code{"Q_constraint"}.
## 
##  Objects from the following classes can be coerced to
##  \code{"Q_constraint"}: \code{"list"}. The \code{"list"} must
##  contain four elements, a list of matrices \eqn{Q_m} representing
##  the quadratic part of \eqn{m} constraints, the matrix \eqn{A}
##  describing the linear part, the direction of the constraints, and
##  the right hand side.
##  @title Quadratic Constraints
##  @param x an R object.
##  @param ... further arguments passed to or from other methods
##  (currently ignored).
##  @return an object of class \code{"Q_constraint"} which inherits
##  from \code{"constraint"}.
##  @author Stefan Theussl
##' @rdname Q_constraint
##' @export
as.Q_constraint <- function( x )
    UseMethod("as.Q_constraint")

##' @noRd
##' @export
as.Q_constraint.Q_constraint <- function(x) {
    if ( is.L_constraint(x) ) return(as.Q_constraint.L_constraint(x))
    return(x)
}

##' @noRd
##' @export
as.Q_constraint.L_constraint <- function( x ) {
    Q_constraint( Q = NULL, L = x$L, dir = x$dir, rhs = x$rhs, names = x$names )
}


##' @noRd
##' @export
as.Q_constraint.list <- function( x ){
    names(x) <- c("Q", "L", "dir", "rhs")
    Q_constraint( Q = x$Q, L = x$L, dir = x$dir, rhs = x$rhs )
}

##  Tests if an object is interpretable as being of class \code{"Q_constraint"}.
## 
##  @title Quadratic Constraints
##  @param x object to be tested.
##  @return returns \code{TRUE} if its argument is of class
##  \code{"Q_constraint"} and \code{FALSE} otherwise.
##  @author Stefan Theussl
##' @rdname Q_constraint
##' @export
is.Q_constraint <- function( x ) {
    inherits( x, "Q_constraint" )
}

rbind_Q_constraint <- function( constraints, use.names=FALSE) {
    constraints <- lapply(constraints, as.Q_constraint)
    if ( use.names ) {
        var.names <- lapply(constraints, function(x) variable.names(x))
        if ( any(sapply(var.names, is.null)) ) {
            stop("all constraints need to be named if 'use.names' is TRUE!")
        }
        is_equal <- function(x, y) isTRUE(all.equal(x, y))
        if ( all(mapply(is_equal, var.names[-1L], var.names[-length(var.names)])) ) {
            return( Reduce(c2_Q_constraints, constraints) )
        } else {
            var.names <- unique(unlist(var.names))
            do_fill <- function(x) lapply(x$Q, fill_stm, old_names=variable.names(x), new_names=var.names)
            Q <- unlist(lapply(constraints, do_fill), recursive=FALSE, use.names=FALSE)
            lc <- L_constraint(simple_triplet_zero_matrix(0L, length(var.names)),
                               NULL, NULL, names=var.names)
            x <- Reduce(c2_L_constraints_named, c(list(lc), constraints))
            return( Q_constraint( Q = Q, L = x$L, dir = x$dir, rhs = x$rhs, names = x$names ) )
        }
    } else {
        return( Reduce(c2_Q_constraints, constraints) )
    }
}

c.Q_constraint <- function( ..., recursive = FALSE )
    rbind( ..., recursive = recursive )

length.Q_constraint <- function(x)
    attr(x, "n_Q_constraints")


##  ----------------------------------------------------------
##  as.Q_term
##  =========
##' @title Coerce an object to type \code{"Q_term"}.
##' @description
##'   The \code{"Q_term"} object represents the quadratic term of the \code{"Q_constraint"}.
##'   Objects from the following classes can be coerced to \code{"Q_term"}:
##'   \code{"NULL"}, \code{"numeric"}, \code{"matrix"}, \code{"simple_triplet_matrix"}
##'   and \code{"list"}.
##' @details
##'   In the case of \code{lists} \code{"as.Q_term"} is applied to every element
##'   of the list, for \code{NULL} one can supply the optional arguments \code{"nrow"}
##'   and \code{"ncol"} which will create a \code{"simple_triplet_zero_matrix"}
##'   with the specified dimension.
##' @param x an R object.
##' @param ... further arguments passed to or from other methods
##' (currently ignored).
##' @return an object of class \code{"simple_triplet_zero_matrix"}
##' @export
as.Q_term <- function(x, ...)
    UseMethod( "as.Q_term" )

##' @rdname as.Q_term
##' @export
as.Q_term.list <- function( x, ... ) {
    dims <- list(...)
    if ( all(c("nrow", "ncol") %in% names(dims)) ) {
        as_Q_term_list <- function(x, nrow, ncol) {
            if ( is.null(x) ) return( as.Q_term(x, nrow=nrow, ncol=ncol) )
            return( as.simple_triplet_matrix(x) )
        }
        lapply(x, as_Q_term_list, nrow=dims$nrow, ncol=dims$ncol)
    }
    return(lapply( x, function(x) if( !is.null(x) ) as.simple_triplet_matrix(x) ))
}
##    lapply( x, function(x) if( !is.null(x) ) as.simple_triplet_matrix(x) )

##' @rdname as.Q_term
##' @export
as.Q_term.numeric <- function( x, ... )
    list( as.simple_triplet_matrix( matrix(x)) )

##' @rdname as.Q_term
##' @export
as.Q_term.matrix <- function( x, ... )
    list( as.simple_triplet_matrix(x) )

##' @rdname as.Q_term
##' @export
as.Q_term.simple_triplet_matrix <- function( x, ... )
    list( x )

##' @rdname as.Q_term
##' @export
as.Q_term.NULL <- function( x, ... ) {
    dims <- list(...)
    if ( all(c("nrow", "ncol") %in% names(dims)) ) {
        return( rep(list(simple_triplet_zero_matrix(dims[['ncol']])), dims[['nrow']]) )
    }
    return( NULL )
}

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
##' @param J an optional \code{function} holding the Jacobian of F.
##' @param x object to be tested.
##' @return an object of class \code{"F_constraint"} which inherits
##' from \code{"constraint"}.
##' @author Stefan Theussl
##' @export
## NOTE: use NA since the J is not available and
##       unlist( list(NA, list(function)) ) => list(NA, function) O.K.
##       unlist( list(NULL, list(function)) ) => list(function)   BAD!
F_constraint <- function(F, dir, rhs, J=NULL){
    F     <- as.F_term( F )
    J     <- as.J_term( J )
    stopifnot( row_sense_is_feasible(dir) )
    rhs   <- as.rhs( rhs )
    n_F   <- length( F )
    n_dir <- length( dir )
    n_F_constraints <- length( rhs )
    ## length of F, dir and rhs need to be equal
    stopifnot( all(c(n_F, n_dir) == n_F_constraints) )
    structure( list(F   = F,
                    dir = dir,
                    rhs = rhs,
                    J   = J),
              n_F_constraints = n_F_constraints,
              class = c("F_constraint", "constraint"))
}

## FIXME: there are still F_constraint methods to implement
as.F_constraint <- function(x, ...)
    UseMethod( "as.F_constraint")

as.F_term <- function(x, ...)
    UseMethod( "as.F_term" )

##' @noRd
##' @export
length.F_constraint <- function(x)
    attr( x, "n_F_constraints" )

as.F_term.function <- function(x, ...)
    list( x )

as.F_term.list <- function(x, ...)
    lapply( x, as.function )

## Jacobian
as.J_term          <- function(x, ...) UseMethod( "as.J_term" )
as.J_term.NULL     <- function(x, ...)      list( NA )
as.J_term.function <- function(x, ...)      list( x )
as.J_term.list     <- function(x, ...) {
    as_J <- function(x) {
        if ( is.null(x) ) return( NA )
        if ( is.function(x) ) return( x )
        if ( is.na(x) ) return( NA )
        return( as.J_term(x) )
    }
    unlist( lapply( x, as_J ), use.names = FALSE )
}

##  Tests if an object is interpretable as being of class \code{"F_constraint"}.
## 
##  @title Function Constraints
##  @param x object to be tested.
##  @return returns \code{TRUE} if its argument is of class
##  \code{"F_constraint"} and \code{FALSE} otherwise.
##' @rdname F_constraint
##' @export
is.F_constraint <- function( x ) {
    inherits( x, "F_constraint" )
}

rbind_F_constraint <- function( constraints ) {
    ## check if all ihnherit from constraint
    b <- sapply(constraints , inherits, what="F_constraint" )
    if ( any(!b) ) stop("object ", paste(which(!b), collapse=", "),
                        "doesn't inherit from F_constraint!")
    fun <- unlist(lapply(constraints, "[[", "F"), use.names=FALSE)
    dir <- unlist(lapply(constraints, "[[", "dir"), use.names=FALSE)
    rhs <- unlist(lapply(constraints, "[[", "rhs"), use.names=FALSE)
    jac <- unlist(lapply(constraints, "[[", "J"), use.names=FALSE)
    ## NOTE: should we check the dims? I currently don't since it should be correct
    ##       as a result of the checks before
    F_constraint(F=fun, dir=dir, rhs=rhs, J=jac)
}

##' @noRd
##' @export
c.F_constraint <- function( ..., recursive = FALSE )
    rbind( ..., recursive = recursive )

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
as.constraint.L_constraint <- identity

##' @noRd
##' @export
as.constraint.Q_constraint <- identity

##' @noRd
##' @export
as.constraint.F_constraint <- identity

##' @noRd
##' @export
as.constraint.numeric <- function( x )
    as.L_constraint( x )

##' @noRd
##' @export
print.constraint <- function( x, ... ){
    len <- length(x)
    if ( is.NO_constraint(x) )
        writeLines( "An object of type 'NO_constraint'." )
    if ( is.L_constraint(x) )
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
        c( length(x), 1 )
    }
    out
}


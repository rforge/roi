################################################################################
## Package: ROI
## File:    OP.R
## Author:  Stefan Theussl
## Changed: 2013-11-25
################################################################################

new_OP <- function() {
    x <- vector("list", 7)
    names(x) <- c("objective", "constraints", "bounds", "types", "maximum",
                  "n_of_variables", "n_of_constraints")
    x[["n_of_variables"]] <- NA_integer_
    x[["n_of_constraints"]] <- NA_integer_
    class(x) <- "OP"
    x
}

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
##' @param x an R object.
##' @return an object of class \code{"OP"}.
##' @examples
##' ## Simple linear program.
##' ## maximize:   2 x_1 + 4 x_2 + 3 x_3
##' ## subject to: 3 x_1 + 4 x_2 + 2 x_3 <= 60
##' ##             2 x_1 +   x_2 +   x_3 <= 40
##' ##               x_1 + 3 x_2 + 2 x_3 <= 80
##' ##               x_1, x_2, x_3 are non-negative real numbers
##'
##' LP <- OP( c(2, 4, 3),
##'           L_constraint(L = matrix(c(3, 2, 1, 4, 1, 3, 2, 2, 2), nrow = 3),
##'                        dir = c("<=", "<=", "<="),
##'                        rhs = c(60, 40, 80)),
##'           max = TRUE )
##' LP
##'
##' ## Simple quadratic program.
##' ## minimize: - 5 x_2 + 1/2 (x_1^2 + x_2^2 + x_3^2)
##' ## subject to: -4 x_1 - 3 x_2       >= -8
##' ##              2 x_1 +   x_2       >=  2
##' ##                    - 2 x_2 + x_3 >=  0
##'
##' QP <- OP( Q_objective (Q = diag(1, 3), L = c(0, -5, 0)),
##'           L_constraint(L = matrix(c(-4,-3,0,2,1,0,0,-2,1),
##'                                   ncol = 3, byrow = TRUE),
##'                        dir = rep(">=", 3),
##'                        rhs = c(-8,2,0)) )
##' QP
##' @author Stefan Theussl
##' @export
OP <- function( objective, constraints, types, bounds, maximum = FALSE ) {
    x <- new_OP()

    if ( !missing(objective) )
        objective(x) <- objective

    constraints(x) <- if ( missing(constraints) ) NULL else constraints
    types(x)       <- if ( missing(types) ) NULL else types
    bounds(x)      <- if ( missing(bounds) ) deferred_bound() else bounds

    maximum(x) <- maximum
        
    x
}

## <<< NOTE: We moved the sanity checks into the objective,
##           bounds, types files. >>>
.check_OP_for_sanity <- function( x ) {
    if ( !is.F_constraint( constraints(x) ) ) {
        if( length( objective(x) ) !=  dim(constraints(x))[2] ) {
            stop( "dimensions of 'objective' and 'constraints' not conformable." )
        }
    }
    len_types <- length( types(x) )
    if( len_types && (len_types > 1L) ) {
        if( length(objective(x)) != len_types ) {
            stop( "dimensions of 'objective' and 'types' not conformable." )
        }
    }
    ## if( !is.null(bounds(x)) )
    ##    if( length(objective(x)) != bounds(x)$nobj )
    ##        stop( "dimensions of 'objective' and 'bounds' not conformable." )
    x
}

##' @noRd
##' @export
print.OP <- function(x, ...) {
    writeLines( "ROI Optimization Problem:\n" )
    ## objective
    len <- length(objective(x))
    op_type <- switch( class(objective(x))[2],
                       "L_objective" = "linear",
                       "Q_objective" = "quadratic",
                       "F_objective" = "nonlinear",
                       "abstract" )
    txt <- sprintf("%s a %s objective function of length %d with",
                   ifelse(x$maximum, "Maximize", "Minimize"), op_type, len)
    writeLines( txt )
    if ( any(types(x) %in% available_types()[-1L]) ) {
        tab <- table(types(x))
        nam <- setNames(c("continuous", "integer", "binary"), c("C", "I", "B"))
        for ( ty in available_types() ) {
            if ( isTRUE(ty %in% names(tab)) ) {
                txt <- sprintf("- %d %s objective variable%s,", 
                               tab[ty], nam[ty], plural_s(tab[ty] != 1L))
                writeLines(txt)
            }
        }
    } else {
        txt <- sprintf("- %d continuous objective variable%s,", 
                       len, plural_s(len != 1L))
        writeLines( txt )
    }
    writeLines( "\nsubject to" )
    ## constraints
    if ( is.NO_constraint(constraints(x)) ) {
        writeLines("- 0 constraints")
    } else {
        types <- c( L_constraint = "linear",
                    Q_constraint = "quadratic",
                    C_constraint = "conic", 
                    F_constraint = "nonlinear" )
        len <- length(constraints(x))
        
        txt <- sprintf("- %d constraint%s of type %s.",
                       len, plural_s(len != 1L),
                       paste(na.omit(types[class(constraints(x))])[1],
                             collapse = ", "))
        writeLines( txt )
        if ( inherits(constraints(x), "C_constraint") ) {
            cones <- table(available_cone_types()[constraints(x)$cones$cone])
            for ( i in seq_along(cones) ) {
                txt <- sprintf("  |- %i conic constraint%s of type '%s'", 
                               cones[i], plural_s(cones[i] != 1), names(cones)[i])
                writeLines( txt )
            }
        }
    }
    if ( !(is.null(bounds(x)$lower) & is.null(bounds(x)$upper)) ) {
        len.lo <- length(bounds(x)$lower$ind)
        len.up <- length(bounds(x)$upper$ind)
        writeLines( sprintf("- %d lower and %d upper non-standard variable bound%s.",
                    len.lo, len.up, plural_s(len.up != 1)) )
    }
}

##  Coerces objects of type \code{"OP"}.
## 
##  Objects from the following classes can be coerced to \code{"OP"}:
##  \code{"numeric"}. This yields an unconstrained linear
##  programming problem where the elements of a \code{"numeric"}
##  vector \eqn{c} are treated as being objective variable
##  coefficients in \eqn{c^\top x}.
##  @title Optimization Problem Object
##  @param x an R object.
##  @return an object of class \code{"OP"}.
##  @author Stefan Theussl
##' @rdname OP
##' @export
as.OP <- function(x)
    UseMethod("as.OP")

##' @noRd
##' @export
as.OP.OP <- identity

##' @noRd
##' @export
as.OP.numeric <- function(x){
    OP( objective = x, constraints = NULL, bounds = NULL, types = NULL,
        maximum = FALSE )
}

## @noRd
## @export
## as.OP.default <- function(x, ...){
##    stop("Method not implemented.")
##}

##' @noRd
## since available_objective_classes are ordered I only need to take the first!
get_objective_class <- function(x) {
    b <- available_objective_classes() %in% class(x$objective)[1]
    names(available_objective_classes())[b]
}

get_constraint_class <- function(x) {
    b <- available_constraint_classes() %in% class(constraints(x))[1]
    names(available_constraint_classes())[b] 
}

get_cone_types <- function(x) {
    cones <- unique(constraints(x)$cones$cone)
    if ( is.null(cones) )
        return("X")
    available_cone_types()[cones]
}

get_varibale_types <- function(x) {
    aty <- available_types()
    if ( is.null(types(x)) ) {
        aty[1]
    } else {
        ## currently the type combinations are c("C", "I", "B", "CI", "CB", "IB", "CIB")
        ## this not ideal since so we can not just collapse        
        paste(aty[aty %in% unique(types(x))], collapse = "")
    }
}

get_bound_type <- function(x) {
    bo <- bounds(x)
    if ( inherits(bo, "V_bound") ) {
        if ( isTRUE(length(bo$upper$ind) == 0L) ) {
            if ( isTRUE(length(bo$lower$ind) == bo$nobj) ) {
                if ( all(bo$lower$val == -Inf) ) {
                    return("X")
                }
            }
        }
        return("V")
    } else {
        return("X")
    }  
}

## NOTE: objective(x) returns something which inherits from function and class(x).
##       this is why we need to derive the type of objective by taking the 2nd element.
##
##  OP_signature
##  ============
##' @title Optimization Problem Signature
##' @description
##'   Takes an object of class \code{"OP"} (optimization problem)
##'   and returns the signature of the optimization problem.
##' @param x an object of class \code{"OP"}
##' @return A \code{data.frame} giving the signature of the
##'         the optimization problem.
##' @export
OP_signature <- function( x ) {
    stopifnot(inherits(x, "OP"))
    ROI_plugin_make_signature( objective = get_objective_class(x),
                               constraints = get_constraint_class(x),
                               types = get_varibale_types(x),
                               bounds  = get_bound_type(x),
                               cones = get_cone_types(x),
                               maximum = x$maximum )
}

#  -----------------------------------------------------------
#  OP_applicable_solver
#  NOTE: is now named ROI_applicable_solvers
#  ====================
#  @title Applicable Solver
#  @description
#    Takes an object of class \code{"OP"} (optimization problem)
#    and returns a character vector giving the names of all available
#    and applicable solver.
#  @param x an object of class \code{"OP"}
#  @return A character vector giving the giving the names of all available
#    and applicable solver
#  @export
#  -----------------------------------------------------------
OP_applicable_solver <- function( x ) {
    unname( names(get_solver_methods( OP_signature(x) )) )
}


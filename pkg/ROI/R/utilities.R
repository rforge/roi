################################################################################
## Package: ROI
## File:    utilities.R
## Author:  Stefan Theussl
## Changed: 2011-10-04
################################################################################



################################################################################
## ROI helper functions
################################################################################

.xtQx <-
function(Q, x)
{
    ## Value of quadratic form t(x) %*% Q %*% x.
    ## As we implement simple triplet matrices in S3, we could only have
    ## %*% and crossprod methods if we created S3 generics for these ...
    if(is.simple_triplet_matrix(Q))
        sum(Q$v * x[Q$i] * x[Q$j])
    else
        c(crossprod(x, Q %*% x))
}

available_row_sense <- function( )
    c("<", "<=", "==", ">", ">=")


################################################################################
## Bounds to linear constraints
################################################################################

## Some plugins do not support variable bounds. the following function
## coerces an object where V_bounds are transformed to constraints and
## inherits from 'OP'.
as.no_V_bounds_OP <- function( x )
    UseMethod("as.no_V_bounds_OP")

##' @noRd
##' @export
as.no_V_bounds_OP.no_V_bounds <- identity

##' @noRd
##' @export
as.no_V_bounds_OP.OP <- function( x ){
    if( length(bounds(x)) )
        constraints(x) <- rbind( constraints(x),
                                 .make_box_constraints_from_bounds(bounds(x),
                                                         length(objective(x))) )
    if( is.null(bounds(x)) )
        constraints(x) <- rbind( constraints(x),
                                 .make_default_box_constraints(length(objective(x))) )
    class( x ) <- c( "no_V_bounds", class(x) )
    x
}

.make_default_box_constraints <- function( n_obj ){
    L_constraint( simple_triplet_diag_matrix(1, n_obj), rep(">=", n_obj), integer(n_obj) )
}

.make_box_constraints_from_bounds <- function( x, n_obj,
                                               reverse = FALSE ) {
    ## create lhs upper bound
    lhs_upper <- simple_triplet_matrix( i = x$upper$ind,
                                        j = x$upper$ind,
                                        v = rep(1, length(x$upper$ind)),
                                        nrow = n_obj,
                                        ncol = n_obj )

    ## if lower indices are not set, we need to set them here
    ## otherwise the constraints will not be built
    if( !length(x$lower$ind) ){
        ## FIXME: shouldn't this also include a "reverse" argument?
        box <- .make_default_box_constraints( n_obj )
        x$lower$ind <- box$L$i
        x$lower$val <- box$rhs
    }

    ## create lhs lower bound
    lhs_lower <-  simple_triplet_matrix( i = x$lower$ind,
                                         j = x$lower$ind,
                                         v = rep(1, length(x$lower$ind)),
                                         nrow = n_obj,
                                         ncol = n_obj )


    ## default constraint direction and multiplicator
    d_l <- ">="
    d_u <- "<="
    m <- 1
    if(reverse){
        ## reverse constraint direction and multiplicator
        d_l<- "<="
        d_u<- ">="
        m <- -1
    }
    rbind( L_constraint(L   = lhs_upper[x$upper$ind, ],
                        dir = rep(d_u, length(x$upper$ind)),
                        rhs = m * x$upper$val),
           L_constraint(L   = lhs_lower[x$lower$ind, ],
                        dir = rep(d_l, length(x$lower$ind)),
                        rhs = m * x$lower$val) )
}

## create box constraints, i.e. lower and upper bounds
## when solver doesn't support this feature
.make_box_constraints_from_bounds_in_MIP <- function(x, negative = TRUE){
    ## FIXME: we really need an extractor for the number of objective vars
    ##        this only works for sure with linear objectives
    n_obj <- length(objective(x))

    if(negative) {
        ## if negative TRUE, then solver defaults are:
        ## lower bound -Inf, upper bound Inf
        constraints(x) <- rbind( constraints(x),
                                .make_box_constraints_from_bounds(bounds(x),
                                                                  n_obj) )
        ## just in case: be sure that solver uses (-oo, oo)
        bounds(x) <- list( lower = list(ind = 1:n_obj, val = rep(-Inf, n_obj)) )

    } else {
        ## if negative FALSE , then solver defaults are
        ## lower bound 0, upper bound Inf (e.g. lpsolve)
        ## TODO: formulate constraints in case the solver only understands
        ##       bounds between 0 and Inf
        if( ! any(bounds(x)$lower$val < 0) ) {
            constraints(x) <- rbind( constraints(x),
                                    .make_box_constraints_from_bounds(bounds(x),
                                                                      n_obj) )
            ##    upper <- bounds(x)$upper
            ##    lower <- bounds(x)$lower
            ##
            ##    ## first: which bounds are nonpositve?
            ##    ind_low_neg <- which( lower$val <= 0 )
            ##        ind_up_neg  <- which( upper$val <= 0 )
            ##    ## lower bounds not included are 0, thus adding
            ##    ind_low_neg <- c( ind_low_neg, (1:n_obj)[ -lower$ind] )
            ##
            ##    both_neg <- ind_up_neg[ind_up_neg %in% ind_low_neg]
            ##    ## this is easy: simple -x_i everwhere
            ##    if(length(both_neg)) {
            ##      constr_both <- .make_box_constraints_from_bounds(
            ##                       V_bound(bounds(x)$lower$ind[both_neg],
            ##                               bounds(x)$upper$ind[both_neg],
            ##                               bounds(x)$lower$val[both_neg],
            ##                               bounds(x)$upper$val[both_neg]),
            ##                       n_obj,
            ##                       reverse = TRUE )
            ##      constraints(x) <- rbind( constraints(x), constr_both )
        } else
        stop("bounds of this type are currently not supported with this solver.")
    }
    x
}



################################################################################
## Types and row sense expander
################################################################################

.expand_row_sense<- function( x, n ) {
    if( is.null(x) ) {
        ## >= by default.
        rep.int(">=", n)
    }
    else {
        if( !row_sense_is_feasible(x) )
            stop( "Invalid direction of constraints." )
        ## Be nicer than necessary ...
        rep( x, length.out = n )
    }
}

.expand_types <- function( x, n ) {
    if( is.null(x) ) {
        ## Continuous by default.
        rep.int("C", n)
    }
    else {
        if( !is.character(x) || !all(x %in% available_types()) )
            stop( "Invalid MIP variable types." )
        ## Be nicer than necessary ...
        rep( x, length.out = n )
    }
}



################################################################################
## Plugin and solver naming
################################################################################

## returns solver name based on package name
## Convention: ROI.plugin.<solver> => <solver>
.plugin_prefix <- function()
    "ROI.plugin"

##  -----------------------------------------------------------
##  get_solver_name
##  ===============
##' @title Get Solver Name
##
##' @description Get the name of the solver plugin.
##' @param pkgname a string giving the package name.
##' @return Returns the name of the solver as character.
##' @export
get_solver_name <- function( pkgname )
    sub(sprintf("%s.", .plugin_prefix()), "", as.character(pkgname))

get_package_name <- function( solver )
    paste(.plugin_prefix(), as.character(solver), sep = ".")


##  -----------------------------------------------------------
##  equal
##  =====
##' @title Compare two Objects
##' @description The utility function \code{equal} can be used to compare two 
##'   \pkg{ROI} objects and is mainly used for testing purposes.
##' @param x an \R object to be compared with object y.
##' @param y an \R object to be compared with object x.
##' @param ... optional arguments to \code{equal}.
##' @return \code{TRUE} if \code{x} and \code{y} are equal \code{FALSE} otherwise.
##' @examples
##' ## compare numeric values
##' equal(1e-4, 1e-5, tol=1e-3)
##' ## L_constraint
##' lc1 <- L_constraint(diag(1), dir=c("=="), rhs=1)
##' lc2 <- L_constraint(diag(2), dir=c("==", "<="), rhs=1:2)
##' equal(lc1, lc1)
##' equal(lc1, lc2)
##' @export
#  -----------------------------------------------------------
equal <- function(x, y, ...) UseMethod("equal")

##' @rdname equal
##' @export
equal.NULL <- function(x, y, ...) {
    return( is.null(x) & is.null(y) )
}

##' @rdname equal
##' @export
equal.logical <- function(x, y, ...) {
    if (length(class(x)) != length(class(y))) return(FALSE)
    if ( any(class(x) != class(y)) ) return(FALSE)
    if (length(x) != length(y)) return(FALSE)
    if ( any(x != y) ) return(FALSE)
    return(TRUE)
}

##' @rdname equal
##' @export
equal.integer <- function(x, y, ...) {
    if (length(class(x)) != length(class(y))) return(FALSE)
    if ( any(class(x) != class(y)) ) return(FALSE)
    if (length(x) != length(y)) return(FALSE)
    if ( any(x != y) ) return(FALSE)
    return(TRUE)
}

##' @rdname equal
##' @export
equal.numeric <- function(x, y, ...) {
    args <- list(...)
    if ( is.null(args$tol) ) args$tol <- 1e-5
    if (length(class(x)) != length(class(y))) return(FALSE)
    if ( any(class(x) != class(y)) ) return(FALSE)
    if (length(x) != length(y)) return(FALSE)
    if ( any( abs(x - y) > args$tol ) ) return(FALSE)
    return(TRUE)
}

##' @rdname equal
##' @export
equal.character <- function(x, y, ...) {
    if (length(class(x)) != length(class(y))) return(FALSE)
    if ( any(class(x) != class(y)) ) return(FALSE)
    if (length(x) != length(y)) return(FALSE)
    if ( any(x != y) ) return(FALSE)
    return(TRUE)
}

##' @rdname equal
##' @export
equal.list <- function(x, y, ...) {
    if ( !equal(class(x), class(y)) ) return(FALSE)
    if (length(x) != length(y)) return(FALSE)
    for (i in seq_along(x)) {
        if ( !equal(x[[i]], y[[i]]) ) return(FALSE)
    }
    return(TRUE)
}

##' @rdname equal
##' @export
equal.simple_triplet_matrix <- function(x, y, ...) {
    if ( !equal(class(x), class(y)) ) return(FALSE)
    if ( !equal(x$nrow, y$nrow) ) return(FALSE)
    if ( !equal(x$ncol, y$ncol) ) return(FALSE)
    xo <- order(x$j, x$i)
    yo <- order(y$j, y$i)
    if ( !equal(x$i[xo], y$i[yo]) ) return(FALSE)
    if ( !equal(x$j[xo], y$j[yo]) ) return(FALSE)
    if ( !equal(x$v[xo], y$v[yo]) ) return(FALSE)
    return(TRUE)
}

##' @rdname equal
##' @export
equal.L_constraint <- function(x, y, ...) {
    if ( !equal(class(x), class(y)) ) return(FALSE)
    if ( !equal(attr(x, "names"), attr(y, "names")) ) return(FALSE)
    if ( !equal(attr(x, "n_L_constraints"), attr(y, "n_L_constraints")) ) return(FALSE)
    if ( !equal(x$dir, y$dir) ) return(FALSE)
    if ( !equal(x$rhs, y$rhs) ) return(FALSE)
    if ( !equal(x$names, y$names) ) return(FALSE)
    if ( !equal(x$L, y$L) ) return(FALSE)
    return(TRUE) 
}

##' @rdname equal
##' @export
equal.Q_constraint <- function(x, y, ...) {
    if ( !equal.L_constraint(x, y) ) return(FALSE)
    if ( !equal(x$Q, y$Q) ) return(FALSE)
    return(TRUE)
}

has.Q_constraint <- function(x) {
    is_Q_constraint_exact <- function(x) ((!is.L_constraint(x)) & is.Q_constraint(x))
    any(sapply(constraints, is_Q_constraint_exact))
}

flatten_constraints <- function(x, message=NULL, domain=NULL) {
    ..X.. <- list()
    flatten_cons <- function(x) {
        if ( inherits(x, "constraint") ) {
            ..X..[[length(..X..) + 1L]] <<- x
        } else if ( is.list(x) ) {
            for ( i in seq_along(x) ) {
                flatten_cons( x[[i]] )
            }
        } else {
            error(which="TYPE_ERROR", message=message, domain=domain, call=NULL)
        }
        return(NULL)
    }
    flatten_cons(x)
    return(..X..)
}

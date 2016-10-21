## gradients
## code based on a patch submitted by Olaf Mersmann.
## slightly modified

##' Extract the gradient from its argument (typically a ROI
##' object of class \code{"objective"}).
##'
##' @title Extract Gradient information
##' @param x an object used to select the method.
##' @param \ldots further arguments passed down to the
##' \code{\link[numDeriv]{grad}()} function for calculating gradients (only for \code{"F_objective"}).
##' @return a \code{"function"}.
##' @author Stefan Theussl
##' @export
G <- function( x, ... )
    UseMethod("G")

## FIXME: HWB suggested (see mail from 25.4.) to allow for using different gradient functions, 
##        e.g. in pracma HWB uses the "central difference formula". st: Implemented via ROI_options. 
##        should be documented how this works
##' @noRd
##' @export
G.F_objective <- function( x, ... ){
    args <- list(...)
    args$func <- terms(x)$F
    g <- terms(x)$G
    if(is.null(g))
        g <- function(x){
            args$x <- x
            do.call(ROI_options("gradient"), args = args)
        }
    stopifnot( is.function(g) )
    g
}

##' @noRd
##' @export
##' @noRd
##' @export
G.L_objective <- function( x, ... ){
    L <- terms(x)$L
    function(x)
        as.numeric(as.matrix(L))
}

##' @noRd
##' @export
G.Q_objective <- function( x, ... ){
    L <- terms(x)$L
    Q <- terms(x)$Q
    function(x)
        c(slam::tcrossprod_simple_triplet_matrix(Q, t(x))) + c(L)
}


## ---------------------------------------------------------
##
##  Jacobian
##  ========
##' @title Extract Jacobian Information
##' @description Derive the Jacobian for a given constraint.
##' @param x a \code{\link{L_constraint}}, \code{\link{Q_constraint}} or 
##'   \code{\link{F_constraint}}.
##' @param \ldots further arguments
##' @return a list of functions
##' @examples
##' L <- matrix(c(3, 4, 2, 2, 1, 2, 1, 3, 2), nrow=3, byrow=TRUE)
##' lc <- L_constraint(L = L, dir = c("<=", "<=", "<="), rhs = c(60, 40, 80))
##' J(lc)
##' @export
## ---------------------------------------------------------
J <- function( x, ... ) UseMethod("J")

##' @rdname J
##' @export
J.L_constraint <- function(x, ...) {
    J_fun <- function(i) {
        g <- as.matrix(x$L[i,])
        return(function(x) as.numeric(g))
    }
    out <- lapply(seq_len(NROW(x$L)), J_fun)
    class(out) <- c(class(out), "jacobian")
    return(out)
}

##' @rdname J
##' @export
J.Q_constraint <- function(x, ...) {
    J_fun <- function(i) {
        L <- terms(x)[['L']][i,]
        Q <- terms(x)[['Q']][[i]]
        return(function(x) {
            c(slam::tcrossprod_simple_triplet_matrix(Q, t(x)) + t(L))
        })
    }
    out <- lapply(seq_len(NROW(x$L)), J_fun)
    class(out) <- c(class(out), "jacobian")
    return(out)
}

##' @noRd
##' @export
print.jacobian <- function(x, ...) print(unclass(x), ...)

## TODO: J.F_constraint, test this function and add it to ROI.plugin.nloptr
##' @noRd
##' @export
J.F_constraint <- function(x, ...) {
    args <- list(...)
    J_fun <- terms(x)$J
    if ( is.null(J_fun) ) {
        fun <- function(func) {
            args$func <- func
            J_fun <- function(x) {
                args$x <- x
                do.call(ROI_options("jacobian"), args = args)
            }
            return(J_fun)
        }
        J_fun <- lapply(terms(x)$F, fun)
        class(J_fun) <- c(class(J_fun), "jacobian")
    }
    stopifnot( all(sapply(J_fun, is.function)) )
    return(J_fun)
}

##' @noRd
##' @export
##  NOTE: returns the jaccobian (not a list of length 1 containing the jaccobian)
J.function <- function(f) {
    args <- list()
    args$func <- f
    jfun <- function(x) {
        args$x <- x
        do.call(ROI_options("jacobian"), args = args)
    }
    return(jfun)
}







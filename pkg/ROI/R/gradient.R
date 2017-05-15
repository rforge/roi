## gradients
## code based on a patch submitted by Olaf Mersmann.
## slightly modified

##' Extract the gradient from its argument (typically a ROI
##' object of class \code{"objective"}).
##'
##' @title Extract Gradient information
##' @param x an object used to select the method.
##' @param \ldots further arguments passed down to the
##'   \code{\link[numDeriv]{grad}()} function for calculating gradients 
##'   (only for \code{"F_objective"}).
##' @details
##'   By default \pkg{ROI} uses the \code{"grad"} function from the 
##'   \pkg{numDeriv} package to derive the gradient information.
##'   An alternative function can be provided via \code{"ROI_options"}.
##'   For example \code{ROI_options("gradient", myGrad)}
##'   would tell \pkg{ROI} to use the function \code{"myGrad"} for the
##'   gradient calculation. The only requirement to the function 
##'   \code{"myGrad"} is that it has the argument \code{"func"}
##'   which takes a function with a scalar real result.
##' @return a \code{"function"}.
##' @examples
##' \dontrun{
##'    f <- function(x) {
##'        return( 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2 )
##'    }
##'    x <- OP( objective = F_objective(f, n=2L), 
##'             bounds = V_bound(li=1:2, ui=1:2, lb=c(-3, -3), ub=c(3, 3)) )
##'    G(objective(x))(c(0, 0)) ## gradient numerically approximated by numDeriv
##'
##'
##'    f.gradient <- function(x) {
##'        return( c( -400 * x[1] * (x[2] - x[1] * x[1]) - 2 * (1 - x[1]),
##'                    200 * (x[2] - x[1] * x[1])) )
##'    }
##'    x <- OP( objective = F_objective(f, n=2L, G=f.gradient), 
##'             bounds = V_bound(li=1:2, ui=1:2, lb=c(-3, -3), ub=c(3, 3)) )
##'    G(objective(x))(c(0, 0)) ## gradient calculated by f.gradient
##' }
##' @export
G <- function( x, ... )
    UseMethod("G")

##  HWB suggested (see mail from 25.4.) to allow for using different gradient 
##      functions, e.g. in pracma HWB uses the "central difference formula". 
##      st: Implemented via ROI_options. 
##      #FIXME: should be documented how this works
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
        as.vector(slam::tcrossprod_simple_triplet_matrix(Q, t(x))) + as.vector(L)
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
            as.vector(slam::tcrossprod_simple_triplet_matrix(Q, t(x))) + as.vector(L)
        })
    }
    out <- lapply(seq_len(NROW(x$L)), J_fun)
    class(out) <- c(class(out), "jacobian")
    return(out)
}

##' @noRd
##' @export
print.jacobian <- function(x, ...) print(unclass(x), ...)

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
J.function <- function(x, ...) {
    args <- list()
    args$func <- x
    jfun <- function(x) {
        args$x <- x
        do.call(ROI_options("jacobian"), args = args)
    }
    return(jfun)
}







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

## FIXME: HWB suggested (see mail from 25.4.) to allow for using different gradient functions, e.g. in pracma HWB uses the "central difference formula". st: Implemented via ROI_options. sould be documented how this works
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
        c(slam::tcrossprod_simple_triplet_matrix(Q, t(x)) + L)
}

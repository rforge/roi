## 
## Mainly to be consistent!
## 

## ---------------------------------------------------------
##
## maximum
## =======
##
## ---------------------------------------------------------
##' @title Extract Direction
##' @description Extract the direction of the optimization problem, i.e.
##'              if we want to minimize or maximize the objective.
##' @param x an object used to select the method.
##' @param value an R object.
##' @return a logical giving the direction.
##' @author Stefan Theussl
##' @export
maximum <- function( x )
    UseMethod("maximum")

##' @noRd
##' @export
maximum <- function( x )
    x$maximum

##' @rdname maximum
##' @export maximum<-
'maximum<-' <- function( x, value )
    UseMethod("maximum<-")

##' @noRd
##' @export
'maximum<-.OP' <- function( x, value ) {
    if (!isTRUE(is.finite(value))) {
        stop("'maximum' needs to be either 'TRUE' or 'FALSE'")
    }
    x[['maximum']] <- as.logical(value)
    x
}

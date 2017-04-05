## 
## Mainly to be consistent!
## 

## ---------------------------------------------------------
##
## maximum
## =======
##
## ---------------------------------------------------------
##' @title Maximum - Accessor and Mutator Functions
##' @description The \link{maximum} of a given optimization problem (\link{OP})
##'     can be accessed or mutated via the method \code{'maximum'}.
##'     If \code{'maximum'} is set to \code{TRUE} the \link{OP} is maximized,
##'     if \code{'maximum'} is set to \code{FALSE} the \link{OP} is minimized.
##' @param x an object used to select the method.
##' @param value an R object.
##' @return a logical giving the direction.
##' @name maximum (Set/Get)
##' @rdname maximum
##' @examples
##' ## maximize: x + y
##' ## subject to: x + y <= 2
##' ## x, y >= 0
##' x <- OP(objective = c(1, 1), 
##'         constraints = L_constraint(L = c(1, 1), dir = "<=", rhs = 2),
##'         maximum = FALSE)
##' maximum(x) <- TRUE
##' maximum(x)
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

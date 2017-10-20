## available types. the first element is the standard type
##   1.  "B"  binary
##   2.  "I"  integer
##   3.  "C"  continuous
available_types <- function( )
    c( "C", "I", "B" )

##  Extract objective variable types from its argument (typically ROI
##  objects) and return them.
## 
##  Currently, there is no default method. For ROI objects of class
##  \code{"OP"} it returns a character vector specifying whether a
##  given objective variable is of type continuous (\code{"C"}),
##  integer (\code{"I"}), or binary (\code{"B"}).
##' @title Types - Accessor and Mutator Functions
##' @description The \link{types} of a given optimization problem (\link{OP}) 
##'     can be accessed or mutated via the method \code{'types'}.
##' @param x an object used to select the method.
##' @param value an R object.
##' @return a character vector.
##' @name types (Set/Get)
##' @rdname types
##' @author Stefan Theussl
##' @examples
##' ## minimize: x + 2 y
##' ## subject to: x + y >= 1
##' ## x, y >= 0    x, y are integer
##' x <- OP(objective = 1:2, constraints = L_constraint(c(1, 1), ">=", 1))
##' types(x) <- c("I", "I")
##' types(x)
##' @export
types <- function( x )
    UseMethod("types")

##' @noRd
##' @export
types.OP <- function( x )
    x$types

##' @rdname types
##' @export types<-
'types<-' <- function( x, value )
    UseMethod("types<-")

##' @noRd
##' @export
'types<-.OP' <- function( x, value ) {
    if ( is.null(value) ) {
        ## do nothing
        ## x["types"] <- list(NULL)
    } else {
        stopifnot(is.character(value), length(value) > 0L)
        if ( is.na(x[["n_of_variables"]]) ) {
            x[["n_of_variables"]] <- length(value)
        } else {
            if ( x[["n_of_variables"]] != length(value) ) {
                stop( "number of variables of 'OP' and 'types' not conformable." )
            }    
        }
        x$types <- as.types(value)
    }   
    x
}

as.types <- function( x )
    UseMethod("as.types")

as.types.character <- function( x ){
    if( !all(x %in% available_types()) ){
        stop("Invalid MIP variable types.")
    }
    x
}

as.types.NULL <- identity

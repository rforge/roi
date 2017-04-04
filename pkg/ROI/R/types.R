## available types. the first element is the standard type
##   1.  "B"  binary
##   2.  "I"  integer
##   3.  "C"  continuous
available_types <- function( )
    c( "C", "I", "B" )

##' Extract objective variable types from its argument (typically ROI
##' objects) and return them.
##'
##' Currently, there is no default method. For ROI objects of class
##' \code{"OP"} it returns a character vector specifying whether a
##' given objective variable is of type continuous (\code{"C"}),
##' integer (\code{"I"}), or binary (\code{"B"}).
##' @title Extract Objective Variable Types
##' @param x an object used to select the method.
##' @param value an R object.
##' @return a character vector.
##' @author Stefan Theussl
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
        x["types"] <- list(NULL)
    } else {
        stopifnot(is.character(value)) 
        if ( length(objective(x)) != length(value) ) {
            stop( "dimensions of 'objective' and 'types' not conformable." )
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

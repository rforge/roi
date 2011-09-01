available_types <- function( )
  c( "C", "I", "B" )

##' @export
types <- function( x )
  UseMethod("types")

##' @method types OP
##' @S3method types OP
types.OP <- function( x )
  x$types


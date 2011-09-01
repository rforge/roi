## objective.R

###############################################################
## objective helper functions

available_objective_classes <- function()
    c(L = "L_objective", Q = "Q_objective", F = "F_objective")

## get objective function from problem object
## returns a function!

##' @export
objective <- function( x )
    UseMethod( "objective" )

##' @method objective default
##' @S3method objective default
objective.default <- function( x )
    as.function( x$objective )

##' @export
as.objective <- function( x )
  UseMethod("as.objective")

##' @method as.objective default
##' @S3method as.objective default
as.objective.default <- function( x )
  as.L_objective( x )

##' @method as.objective objective
##' @S3method as.objective objective
as.objective.objective <-
    identity

##' @method terms function
##' @S3method terms function
terms.function <- function( x, ... ){
    if( inherits(x, "L_objective") )
        return( terms(as.L_objective(x)) )
    if( inherits(x, "Q_objective") )
        return( terms(as.Q_objective(x)) )
    NA
}

##' @method terms L_objective
##' @S3method terms L_objective
terms.L_objective <- function( x, ... )
  list( L = x$L )

##' @method terms Q_objective
##' @S3method terms Q_objective
terms.Q_objective <- function( x, ... )
  list( Q = x$Q, L = x$L )

###############################################################
## linear objectives

##' @export
L_objective <- function( L ) {
  structure( list(L = as.numeric(L)),
             class = c("L_objective", "objective") )
}

##' @method as.function L_objective
##' @S3method as.function L_objective
as.function.L_objective <- function( x, ... ){
  L <- terms(x)[["L"]]
  out <- function(x)
    crossprod(L, x)
  class(out) <- c(class(out), class(x))
  out
}

##' @export
as.L_objective <- function( x )
    UseMethod( "as.L_objective" )

##' @method as.L_objective L_objective
##' @S3method as.L_objective L_objective
as.L_objective.L_objective <- identity

##' @method as.L_objective NULL
##' @S3method as.L_objective NULL
as.L_objective.NULL <- function( x )
    L_objective( x )

##' @method as.L_objective numeric
##' @S3method as.L_objective numeric
as.L_objective.numeric <- function( x )
    L_objective( x )

##' @method as.L_objective Q_objective
##' @S3method as.L_objective Q_objective
as.L_objective.Q_objective <- function( x )
    L_objective( terms(x)[["L"]])

##' @method as.L_objective function
##' @S3method as.L_objective function
as.L_objective.function <- function( x ){
    if( !inherits(x, "objective") )
        stop("'x' must be a function which inherits from 'objective'")
    L_objective( get("L", environment(x)) )
}

###############################################################
## quadratic objectives

##' @export
Q_objective <- function( Q, L = NULL ) {

  structure ( list(Q = as.simple_triplet_matrix(0.5 * (Q + t(Q))),
                   L = as.numeric(L)),
              class = c("Q_objective", "objective") )
}

##' @method as.function Q_objective
##' @S3method as.function Q_objective
as.function.Q_objective <- function( x, ... ){
  L <- terms(x)[["L"]]
  Q <- terms(x)[["Q"]]
  out <- function(x)
    crossprod(L, x) + 0.5 * .xtQx(Q, x)
  class(out) <- c(class(out), class(x))
  out
}

##' @export
as.Q_objective <- function(x, ...)
  UseMethod("as.Q_objective")

##' @method as.Q_objective function
##' @S3method as.Q_objective function
as.Q_objective.function <- function( x, ... ){
  if( !inherits(x, "objective") )
    stop( "'x' must be a function which inherits from 'objective'" )
  L_objective( get("L", environment(x)) )
  Q_objective( L = get("L", environment(x)),
               Q = get("Q", environment(x)) )
}

##' @method as.Q_objective matrix
##' @S3method as.Q_objective matrix
as.Q_objective.matrix <- function( x, ... )
  Q_objective( Q = x)

##' @method as.Q_objective numeric
##' @S3method as.Q_objective numeric
as.Q_objective.numeric <- function( x, ... )
  Q_objective( Q = matrix(x))

##' @method as.Q_objective Q_objective
##' @S3method as.Q_objective Q_objective
as.Q_objective.Q_objective <- function( x, ... )
  identity(x)

##' @method as.Q_objective simple_triplet_matrix
##' @S3method as.Q_objective simple_triplet_matrix
as.Q_objective.simple_triplet_matrix <- function( x, ... )
  Q_objective(Q = x)

###############################################################
## general objectives

F_objective <- function( F ) {
  structure ( list(F = as.function(F)),
             class = c("F_objective", "objective") )
}

as.function.F_objective <- function( x, ... )
  x$F

as.F_objective <- function(x, ...)
  UseMethod("as.F_objective")

as.F_objective.F_objective <- function(x, ... )
    identity( x )

as.F_objective.L_objective<- function( x, ... )
  F_objective( x )

as.F_objective.Q_objective<- function( x, ... )
  F_objective( x )

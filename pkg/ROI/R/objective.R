## objective.R

###############################################################
## objective helper functions

## get objective function from problem object
## returns a function!
## FIXME: use a super class 'optimization_problem'?

objective <- function( x )
  UseMethod("objective")

objective.default <- function( x )
  as.function( x$objective )

terms.function <- function( x, ... ){
  if(inherits(x, "L_objective"))
    return( terms(as.L_objective(x)) )
  if(inherits(x, "Q_objective"))
    return( terms(as.Q_objective(x)) )
  NA
}

terms.L_objective <- function( x, ... )
  list( L = x$L )

terms.Q_objective <- function( x, ... )
  list( Q = x$Q, L = x$L )

###############################################################
## linear objectives

L_objective <- function( L ) {
  structure( list(L = as.numeric(L)),
             class = c("L_objective", "objective") )
}

as.function.L_objective <- function( x, ... ){
  L <- terms(x)[["L"]]
  out <- function(x)
    crossprod(L, x)
  class(out) <- c(class(out), class(x))
  out
}

as.L_objective <- function(x, ...)
  UseMethod("as.L_objective")

as.L_objective.L_objective <- function( x, ... )
  identity(x)

as.L_objective.numeric <- function( x, ... )
  L_objective( x )

as.L_objective.Q_objective <- function( x, ... )
  L_objective( terms(x)[["L"]])

as.L_objective.function <- function( x, ... ){
  if( !inherits(x, "objective") )
    stop("'x' must be a function which inherits from 'objective'")
  L_objective( get("L", environment(x)) )
}

###############################################################
## quadratic objectives

Q_objective <- function( Q, L = NULL ) {

  structure ( list(Q = as.simple_triplet_matrix(0.5 * (Q + t(Q))),
                   L = as.numeric(L)),
              class = c("Q_objective", "objective") )
}

as.function.Q_objective <- function( x, ... ){
  L <- terms(x)[["L"]]
  Q <- terms(x)[["Q"]]
  out <- function(x)
    crossprod(L, x) + 0.5 * .xtQx(Q, x)
  class(out) <- c(class(out), class(x))
  out
}

as.Q_objective <- function(x, ...)
  UseMethod("as.Q_objective")

as.Q_objective.function <- function( x, ... ){
  if( !inherits(x, "objective") )
    stop( "'x' must be a function which inherits from 'objective'" )
  L_objective( get("L", environment(x)) )
  Q_objective( L = get("L", environment(x)),
               Q = get("Q", environment(x)) )
}

as.Q_objective.matrix <- function( x, ... )
  Q_objective( Q = x)

as.Q_objective.numeric <- function( x, ... )
  Q_objective( Q = matrix(x))

as.Q_objective.Q_objective <- function( x, ... )
  identity(x)

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

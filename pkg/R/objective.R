## objective.R

###############################################################
## objective helper functions

## get objective function from problem object
## returns a function!
## FIXME: use a super class 'optimization_problem'?
objective.MILP <- function( x )
  as.function( x$objective )

objective.MIQP <- function( x )
  as.function( x$objective )

objective.MIQCP <- function( x )
  as.function( x$objective )

objective.MINLP <- function( x )
  as.function( x$objective )

terms.L_objective <- function( x )
  x$L

terms.Q_objective <- function( x )
  list( x$Q, x$L )

###############################################################
## linear objectives

L_objective <- function( L ) {
  structure( list(L = as.numeric(L)),
             class = c("L_objective", "objective") )
}

as.function.L_objective <- function( L ){
  L <- terms(L)
  function(x)
    crossprod(L, x) 
}

as.L_objective.L_objective <- identity

as.L_objective.numeric <- function( x )
  L_objective( x )

as.L_objective.Q_objective <- function( x )
  L_objective( terms(x)$L )

as.L_objective.function <- function( x )
  L_objective( get("L", environment(x)) )
  
###############################################################
## quadratic objectives

Q_objective <- function( Q, L = NULL ) {
  
  structure ( list(L = as.numeric(L),
                   Q = as.simple_triplet_matrix(Q)),
              class = c("Q_objective", "objective") )
}

as.function.Q_objective <- function( Q ){
  L <- terms(Q)$L
  Q <- terms(Q)$Q
  function(x)
    crossprod(L, x) + t(x) %*% Q %*% x  
}

as.Q_objective.Q_objective <- identity

as.Q_objective.numeric <- function( x )
  Q_objective(matrix(x))

as.Q_objective.matrix <- function( x )
  Q_objective(x)

as.Q_objective.simple_triplet_matrix <- function( x )
  Q_objective(x)
  
###############################################################
## general objectives

F_objective <- function( F ) {
  structure ( list(F = as.function(F)),
             class = c("F_objective", "objective") ) 
}

as.function.F_objective <- function( F )
  F$F

as.F_objective.F_objective <- identity

as.F_objective.L_objective<- function( x )
  F_objective( x )

as.F_objective.Q_objective<- function( x )
  F_objective( x )


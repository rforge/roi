## generics.R
## function definitions of all generics
## listed alphabetically

as.constraint <- function( x, ... )
    UseMethod("as.constraint")

as.F_constraint <- function(x, ...)
  UseMethod("as.F_constraint")

as.F_objective <- function(x, ...)
  UseMethod("as.F_objective")

as.F_term <- function(x, ...)
  UseMethod( "as.F_term" )

as.L_constraint <- function(x, ...)
  UseMethod("as.L_constraint")

as.L_objective <- function(x, ...)
  UseMethod("as.L_objective")

as.L_term <- function( x, ... )
  UseMethod("as.L_term")

as.LP <- function(x, ...)
  UseMethod("as.LP")

as.MILP <- function(x, ...)
  UseMethod("as.MILP")

as.Q_constraint <- function(x, ...)
  UseMethod("as.Q_constraint")

as.Q_objective <- function(x, ...)
  UseMethod("as.Q_objective")

as.Q_term <- function(x, ...)
  UseMethod( "as.Q_term" )

as.rhs <- function(x, ...)
  UseMethod("as.rhs")

constraints <- function(x, ...)
  UseMethod("constraints")

objective <- function(x, ...)
  UseMethod("objective")

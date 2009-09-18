## constraints.R

###############################################################
## constraint helper functions

constraints.MILP <- function(x)
  x$constraints

as.rhs.numeric <- function(x)
  x

available_row_sense <- function() {
  c('<', '<=', "==", ">", ">=")
}

row_sense_is_feasible <- function(x) {
  all( x %in% available_row_sense() )
}

###############################################################
## Linear constraints (class 'L_constraint')
##  Ax ~ b

L_constraint <- function( L, dir, rhs ) {
  L     <- as.L_term(L)
  stopifnot( row_sense_is_feasible(dir) )
  rhs   <- as.rhs( rhs )
  dim_L <- dim( L )
  n_dir <- length( dir )
  n_L_constraints <- length( rhs )
  stopifnot( all(c(dim_L[ 1 ], n_dir) == n_L_constraints) )
  structure( list(L   = L,
                  dir = dir,
                  rhs = rhs,
                  n_L_constraints = n_L_constraints),
            class = c("L_constraint", "constraint") )
}

## the linear term of the left hand side

as.L_term.numeric <- function(x)
  as.simple_triplet_matrix( matrix(x, nrow = 1L) )

as.L_term.matrix <- function(x)
  as.simple_triplet_matrix(x)

as.L_term.simple_triplet_matrix <- function(x)
  x

###############################################################
## Quadratic constraints (class 'Q_constraint')
## list of constraints of the form a'x + x'Qx ~ b

Q_constraint <- function(Q, L, dir, rhs){
  Q     <- as.Q_term( Q )
  L     <- as.L_term( L )
  stopifnot( row_sense_is_feasible(dir) )
  rhs   <- as.rhs( rhs )
  dim_L <- dim( L )
  n_Q   <- length( Q )
  dim_Q <- lapply( Q, dim )
  n_dir <- length( dir )
  n_Q_constraints <- length( rhs )
  ## all Q need to be nxn and L kxn
  stopifnot( all(unlist(dim_Q) == dim_L[ 2 ]) )
  ## length of dir and rhs, as well as rows of L need to be equal
  stopifnot( all(c(dim_L[ 1 ], n_dir) == n_Q_constraints) )
  structure( list(Q   = Q,
                  L   = L,
                  dir = dir,
                  rhs = rhs,
                  n_Q_constraints = n_Q_constraints),
             class = c("Q_constraint", "constraint"))
}

## the quadratic term of the left hand side

as.Q_term.numeric <- function(x)
  list( as.simple_triplet_matrix(matrix(x)) )
  
as.Q_term.matrix <- function(x)
  list( as.simple_triplet_matrix(x) )

as.Q_term.simple_triplet_matrix <- function(x)
  list( x )

as.Q_term.list <- function(x)
  lapply( x, as.simple_triplet_matrix )

## combine, print, and summary methods

##summary.Q_constraint <- function(x){
##  
##}

##c.Q_constraint <- function( ... ){
##  Q_constraint()
##}

###############################################################
## Function constraints (class 'F_constraint')
## list of constraints of the form f(x) ~ b

F_constraint <- function(F, dir, rhs){
  F     <- as.F_term( F )
  stopifnot( row_sense_is_feasible(dir) )
  rhs   <- as.rhs( rhs )
  n_F   <- length( F )
  n_dir <- length( dir )
  n_F_constraints <- length( rhs )
  ## length of F, dir and rhs need to be equal
  stopifnot( all(c(n_F, n_dir) == n_F_constraints) )
  structure( list(F   = F,
                  dir = dir,
                  rhs = rhs,
                  n_F_constraints = n_F_constraints),
            class = c("F_constraint", "constraint"))
}

as.F_term.function <- function(x)
  list( x )

as.F_term.list <- function(x)
  lapply( x, as.function )



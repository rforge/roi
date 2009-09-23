## constraints.R

###############################################################
## constraint helper functions

constraints.MILP <- function( x, ... )
  x$constraints

constraints.MIQP <- function( x, ... )
  x$constraints

constraints.MINLP <- function( x, ... )
  x$constraints

as.rhs.numeric <- function( x, ... )
  x

as.constraint.L_constraint <- function( x, ... )
  identity(x)

as.constraint.Q_constraint <- function( x, ... )
  identity(x)

as.constraint.F_constraint <- function( x, ... )
  identity(x)


##c.constraint <- function( ..., recursive = FALSE ) {
##  constraints <- lapply(list(...), as.constraint)
##  any(is.NCP
##  constraints
##}

print.constraint <- function( x, ... ){
  len <- length(x)
  if( is.NCP(x) )
    writeLines( c(sprintf("A set of %d constraints.", len),
                  "Some constraints are of type nonlinear.") )
  else
    if( is.QCP(x) )
      writeLines( c(sprintf("A set of %d constraints.", len),
                  "Some constraints are of type quadratic.") )
  
    else
      writeLines( sprintf("A set of %d linear constraints.", len) )
  
  invisible(x)
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

as.L_constraint.L_constraint <- function( x, ... )
  identity(x)

as.L_constraint.numeric <- function( x, ... )
  L_constraint( L = x, dir = ">=", rhs = 0 )

as.L_constraint.list <- function( x, ... ){
  names(x) <- c("L", "dir", "rhs")
  L_constraint( L = x$L, dir = x$dir, rhs = x$rhs )
}

length.L_constraint <- function( x )
  x$n_L_constraints

## the linear term of the left hand side

as.L_term.numeric <- function( x, ... )
  as.simple_triplet_matrix( matrix(x, nrow = 1L) )

as.L_term.matrix <- function( x, ... )
  as.simple_triplet_matrix(x)

as.L_term.simple_triplet_matrix <- function( x, ... )
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
             class = c("Q_constraint", "constraint") )
}

as.Q_constraint.Q_constraint <- function( x, ... )
  identity(x)

as.Q_constraint.list <- function( x, ... ){
  names(x) <- c("Q", "L", "dir", "rhs")
  Q_constraint( Q = x$Q, L = x$L, dir = x$dir, rhs = x$rhs )
}

length.Q_constraint <- function(x)
  x$n_Q_constraints

## the quadratic term of the left hand side

as.Q_term.list <- function( x )
  lapply( x, as.simple_triplet_matrix )

as.Q_term.numeric <- function( x )
  list( as.simple_triplet_matrix( matrix(x)) )
  
as.Q_term.matrix <- function( x )
  list( as.simple_triplet_matrix(x) )

as.Q_term.simple_triplet_matrix <- function( x )
  list( x )

## combine, print, and summary methods

##summary.Q_constraint <- function(x){
##  
##}

##c.Q_constraint <- function( ... ){
##  Q_constraint()
##}

is.QCP <- function(x)
  inherits(x, "Q_constraint")

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

length.F_constraint <- function(x)
  x$n_F_constraints

as.F_term.function <- function(x)
  list( x )

as.F_term.list <- function(x)
  lapply( x, as.function )

## does the constraint object include nonlinear constraints
is.NCP <- function(x)
  inherits(x, "F_constraint")

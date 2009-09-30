## constraints.R
## includes 'bounds' and 'constraints'

################################################################################
## 'bounds'

################################################################################
## 'V_bound' constructor

## li   ... lower bound indices
## ui   ... upper bound indices
## lb   ... lower bound values
## ub   ... upper bound values
## nobj ... number of objective variables
V_bound <- function( li, ui, lb, ub, nobj = max(li, ui) ) {
  li <- as.integer(li)
  ui <- as.integer(ui)
  lb <- as.double(lb)
  ub <- as.double(ub)
  ## Sanity checking
  if( (length(li) != length(lb)) || (length(ui) != length(ub)) )
    stop("length of indices must be equal to the length of the corresponding values.")
  if( any(duplicated(li)) || any(duplicated(ui)) )
    stop("duplicated entries in indices.")
  if( (max(li) > nobj) || (max(ui) > nobj) )
    stop("indices must not exceed number of objective coefficients.")
    if( any(lb >= Inf) )
      stop("lower bound cannot be 'Inf'.")
  if( any(ub <= -Inf) )
      stop("upper bounds cannot be '-Inf'.")
  ## FIXME: lower bounds vs. upper bounds -> lb cannot be higher than ub and
  ##        the other way round
  structure( list(lower = list(ind = li, val = lb),
                  upper = list(ind = ui, val = ub),
                  nobj = as.integer(nobj)),
            class = "V_bound" )
}

as.list.V_bound <- function( x )
  unclass( x )

################################################################################
## 'bounds' extractor functions

bounds.LP <- function( x )
  x$bounds

bounds.QCP <- function( x )
  x$bounds

bounds.QP <- function( x )
  x$bounds

bounds.MILP <- function( x )
  x$bounds

bounds.MIQCP <- function( x )
  x$bounds

bounds.MIQP <- function( x )
  x$bounds

bounds.MINLP <- function( x )
  x$bounds

################################################################################
## 'constraints' replacement functions

'bounds<-.LP' <- function( x, value ) {
  if(is.null(value))
    value <- .make_empty_bounds()
  x$bounds <- value
  x
}

'bounds<-.QCP' <- function( x, value ) {
  if(is.null(value))
    value <- .make_empty_bounds()
  x$bounds <- value
  x
}

'bounds<-.QP' <- function( x, value ) {
  if(is.null(value))
    value <- .make_empty_bounds()
  x$bounds <- value
  x
}

'bounds<-.MILP' <- function( x, value ){
  if(is.null(value))
    value <- .make_empty_bounds()
  x$bounds <- value
  x
}

'bounds<-.MIQCP' <- function( x, value ){
  if(is.null(value))
    value <- .make_empty_bounds()
  x$bounds <- value
  x
}

'bounds<-.MIQP' <- function( x, value ){
  if(is.null(value))
    value <- .make_empty_bounds()
  x$bounds <- value
  x
}

'bounds<-.MINLP' <- function( x, value ){
  if(is.null(value))
    value <- .make_empty_bounds()
  x$bounds <- value
  x
}

                    
################################################################################
## 'constraints'

################################################################################
## 'constraints' extractor functions

## we definitely need a superclass to facilitate extraction
constraints.LP <- function( x )
  x$constraints

constraints.QCP <- function( x )
  x$constraints

constraints.QP <- function( x )
  x$constraints

constraints.MILP <- function( x )
  x$constraints

constraints.MIQCP <- function( x )
  x$constraints

constraints.MIQP <- function( x )
  x$constraints

constraints.MINLP <- function( x )
  x$constraints

################################################################################
## 'constraints' replacement functions

'constraints<-.LP' <- function( x, value ) {
  if(is.null(value))
    value <- L_constraint(L = NULL, dir = NULL, rhs = NULL)
  x$constraints <- as.constraint(value)
  x
}

'constraints<-.QCP' <- function( x, value ) {
  if(is.null(value))
    value <- Q_constraint(Q = NULL, L = NULL, dir = NULL, rhs = NULL)
  x$constraints <- as.constraint(value)
  x
}

'constraints<-.QP' <- function( x, value ) {
  if(is.null(value))
    value <- L_constraint(L = NULL, dir = NULL, rhs = NULL)
  x$constraints <- as.constraint(value)
  x
}

'constraints<-.MILP' <- function( x, value ){
  if(is.null(value))
    value <- L_constraint(L = NULL, dir = NULL, rhs = NULL)
  x$constraints <- as.constraint(value)
  x
}

'constraints<-.MIQCP' <- function( x, value ){
  if(is.null(value))
    value <- Q_constraint(Q = NULL, L = NULL, dir = NULL, rhs = NULL)
  x$constraints <- as.constraint(value)
  x
}

'constraints<-.MIQP' <- function( x, value ){
  if(is.null(value))
    value <- L_constraint(L = NULL, dir = NULL, rhs = NULL)
  x$constraints <- as.constraint(value)
  x
}

'constraints<-.MINLP' <- function( x, value ){
  if(is.null(value))
    value <- F_constraint(F = NULL, dir = NULL, rhs = NULL)
  x$constraints <- as.constraint(value)
  x
}

################################################################################
## constraint helper functions

as.rhs.numeric <- function( x, ... )
  x

as.constraint.L_constraint <- function( x, ... )
  identity(x)

as.constraint.Q_constraint <- function( x, ... )
  identity(x)

as.constraint.F_constraint <- function( x, ... )
  identity(x)

.make_empty_bounds <- function( x )
  NULL

print.constraint <- function( x, ... ){
  len <- length(x)
  if( is.L_constraint(x) )
    writeLines( sprintf("An object containing %d linear constraints.", len) )
  else
    if( is.Q_constraint(x) )
      writeLines( c(sprintf("An object containing %d constraints.", len),
                            "Some constraints are of type quadratic.") )
    else
      writeLines( c(sprintf("An object containing %d constraints.", len),
                            "Some constraints are of type nonlinear.") )
  
  invisible(x)
}


################################################################################
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

is.L_constraint <- function( x ) {
  inherits( x, "L_constraint" )
}

## combining matrices (see 'rbind' in matrix.R, package relation)
rbind.L_constraint <- function( ..., recursive = FALSE ){
  constraints <- lapply(list(...), as.L_constraint)
  L   <- lapply( constraints, function (x) as.simple_triplet_matrix(x$L) )
  dir <- lapply( constraints, function (x) as.character(x$dir) )
  rhs <- lapply( constraints, function (x) as.rhs(x$rhs) )
  L_constraint( L =   Reduce(function(x, y) rbind(x, y), L),
                dir = Reduce(function(x, y) c(x, y), dir),
                rhs = Reduce(function(x, y) c(x, y), rhs) )
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

################################################################################
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

is.Q_constraint <- function( x ) {
  inherits( x, "Q_constraint" )
}

length.Q_constraint <- function(x)
  x$n_Q_constraints

## the quadratic term of the left hand side

as.Q_term.list <- function( x )
  lapply( x, function(x) if( !is.null(x) ) as.simple_triplet_matrix(x) )

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

################################################################################
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

## create box constraints, i.e. lower and upper bounds
## when solver doesn't support this feature

.make_box_constraints_from_bounds_in_MIP <- function(x, negative = TRUE){
  ## FIXME: we really need an extractor for the number of objective vars
  ##        this only works for sure with linear objectives
  n_obj <- length(terms(objective(x))$L)

  if(negative) {
    ## if negative TRUE, then solver defaults are:
    ## lower bound -Inf, upper bound Inf
    constraints(x) <- rbind( constraints(x),
                             .make_box_constraints_from_bounds(bounds(x),
                                                               n_obj) )
    ## just in case: be sure that solver uses (-oo, oo)
    bounds(x) <- list( lower = list(ind = 1:n_obj, val = rep(-Inf, n_obj)) )
    
  } else {
    ## if negative FALSE , then solver defaults are
    ## lower bound 0, upper bound Inf (e.g. lpsolve)
    ## TODO: formulate constraints in case the solver only understands
    ##       bounds between 0 and Inf
    if( ! any(bounds(x)$lower$val < 0) ) {
      constraints(x) <- rbind( constraints(x),
                              .make_box_constraints_from_bounds(bounds(x),
                                                                n_obj) )
##    upper <- bounds(x)$upper
##    lower <- bounds(x)$lower
##   
##    ## first: which bounds are nonpositve?
##    ind_low_neg <- which( lower$val <= 0 )
##        ind_up_neg  <- which( upper$val <= 0 )
##    ## lower bounds not included are 0, thus adding
##    ind_low_neg <- c( ind_low_neg, (1:n_obj)[ -lower$ind] )
##    
##    both_neg <- ind_up_neg[ind_up_neg %in% ind_low_neg]
##    ## this is easy: simple -x_i everwhere
##    if(length(both_neg)) {
##      constr_both <- .make_box_constraints_from_bounds(
##                       V_bound(bounds(x)$lower$ind[both_neg],
##                               bounds(x)$upper$ind[both_neg],
##                               bounds(x)$lower$val[both_neg],
##                               bounds(x)$upper$val[both_neg]),
##                       n_obj,
##                       reverse = TRUE )
##      constraints(x) <- rbind( constraints(x), constr_both )
    } else
    stop("bounds of this type are currently not supported with this solver.")
  }
  x
}

.make_box_constraints_from_bounds <- function( x, n_obj,
                                               reverse = FALSE ) {
  ## create lhs upper bound 
  lhs_upper <- simple_triplet_matrix( i = x$upper$ind,
                                      j = x$upper$ind,
                                      v = rep(1, length(x$upper$ind)),
                                      nrow = n_obj,
                                      ncol = n_obj )
  ## create lhs lower bound 
  lhs_lower <- simple_triplet_matrix( i = x$lower$ind,
                                      j = x$lower$ind,
                                      v = rep(1, length(x$lower$ind)),
                                      nrow = n_obj,
                                      ncol = n_obj )
  ## default constraint direction and multiplicator 
  d_l <- ">="
  d_u <- "<="
  m <- 1
  if(reverse){
    ## reverse constraint direction and multiplicator 
    d_l<- "<="
    d_u<- ">="
    m <- -1
  }
  rbind( L_constraint(L = lhs_upper[x$upper$ind, ],
                      dir = rep(d_u, length(x$upper$ind)),
                      rhs = m * x$upper$val),
        L_constraint(L = lhs_lower[x$lower$ind, ],
                     dir = rep(d_l, length(x$lower$ind)),
                     rhs = m * x$lower$val) )
}


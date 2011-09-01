################################################################################
available_constraint_classes <- function()
    c(L = "L_constraint", Q = "Q_constraint", F = "F_constraint", X = "NO_constraint")

## 'constraints' extractor functions

##' Extract constraints from its argument (typically ROI objects) and
##' return them.
##'
##' Currently, there is no default method. See \code{\link{constraints.OP}}
##' for extracting constraints from ROI objects of class \code{"OP"}.
##' @title Extract constraints
##' @param x an object used to select the method.
##' @return the extracted constraints object.
##' @author Stefan Theussl
##' @export
constraints <- function( x )
  UseMethod("constraints")

##' Extract constraints from ROI objects of class \code{"OP"} and
##' return them.
##'
##'
##' @title Extract constraints
##' @param x an object of class \code{"OP"}.
##' @return an object inheriting from class \code{"constraints"}.
##' @author Stefan Theussl
##' @method constraints OP
##' @S3method constraints OP
constraints.OP <- function( x )
  x$constraints


################################################################################
## 'constraints' replacement functions

##' Replaces the constraints in R objects (typically ROI
##' objects).
##'
##' Currently, there is no default method. See \code{\link{constraints<-.OP}}
##' for replacing constraints in ROI objects of class \code{"OP"}.
##' @title Replacement of constraints
##' @name constraints-replace
##' @aliases constraints<-
##' @param x an R object.
##' @param value an R object.
##' @return the updated object.
##' @author Stefan Theussl
##' @export constraints<-
'constraints<-' <- function( x, value )
  UseMethod("constraints<-")

##' Replaces the constraints element in a ROI object of class \code{"OP"}
##' with the new constraints object given by \code{value}.
##'
##' @title Replacement of constraints
##' @name constraints-replace-OP
##' @aliases constraints<-.OP
##' @param x an object of class \code{"OP"}.
##' @param value an object of class \code{"constraints"}, coercible to
##' such, or NULL (no constraints).
##' @return the updated \code{"OP"} object.
##' @author Stefan Theussl
##' @method constraints<- OP
##' @S3method constraints<- OP
'constraints<-.OP' <- function( x, value ) {
    ## if 'empty' constraints are given (NULL) then we assume it's and
    ## 'empty' linear constraint
    if( is.null(value) )
        value <- L_constraint(L = NULL, dir = NULL, rhs = NULL)
   x$constraints <- as.constraint(value)
   x
}

################################################################################
## Linear constraints (class 'L_constraint')
##  Ax ~ b

##' Linear constraints are typically of the form \eqn{Ax \leq
##' b}. \eqn{A} is a (sparse) matrix of coefficients to the objective
##' variables \eqn{x}. \eqn{b} is called the right hand side of the
##' constraints.
##'
##' @title Linear Constraints
##' @param L a numeric vector of length \eqn{n} (a single constraint)
##' or a matrix of dimension \eqn{n \times m}, where \eqn{n} is the
##' number of objective variables and \eqn{m} is the number of
##' constraints. Matrices can be of class
##' \code{"simple_triplet_matrix"} to allow a sparse representation of
##' constraints.
##' @param dir a character vector with the directions of the
##' constraints. Each element must be one of \code{"<"}, \code{"<="},
##' \code{">"}, \code{">="}, \code{"=="} or \code{"!="}.
##' @param rhs a numeric vector with the right hand side of the constraints.
##' @return an object of class \code{"L_constraint"} which inherits
##' from \code{"constraint"}.
##' @author Stefan Theussl
##' @export
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
            class = c("L_constraint", "Q_constraint", "constraint") )
}

##' Coerces objects of type \code{"L_constraint"}.
##'
##' Objects from the following classes can be coerced to
##' \code{"L_constraint"}: \code{"numeric"} and \code{"list"}. The
##' elements of a \code{"numeric"} vector \eqn{a} are treated as
##' objective variable coefficients of a single constraint in standard
##' form (\eqn{ax \geq 0}). A \code{"list"} must contain three
##' elements, the matrix \eqn{A}, the direction of constraints, and
##' the right hand side defining a linear constraint \eqn{Ax \leq b}.
##' @title Linear Constraints
##' @param x an R object.
##' @param ... further arguments passed to or from other methods
##' (currently ignored).
##' @return an object of class \code{"L_constraint"} which inherits
##' from \code{"constraint"}.
##' @author Stefan Theussl
##' @export
as.L_constraint <- function(x, ...)
  UseMethod("as.L_constraint")

##' @method as.L_constraint L_constraint
##' @S3method as.L_constraint L_constraint
as.L_constraint.L_constraint <- function( x, ... )
  identity(x)

##' @method as.L_constraint numeric
##' @S3method as.L_constraint numeric
as.L_constraint.numeric <- function( x, ... )
  L_constraint( L = x, dir = ">=", rhs = 0 )

##' @method as.L_constraint list
##' @S3method as.L_constraint list
as.L_constraint.list <- function( x, ... ){
  names(x) <- c("L", "dir", "rhs")
  L_constraint( L = x$L, dir = x$dir, rhs = x$rhs )
}

##' Tests if an object is interpretable as being of class \code{"L_constraint"}.
##'
##' @title Linear Constraints
##' @param x object to be tested.
##' @return returns \code{TRUE} if its argument is of class
##' \code{"L_constraint"} and \code{FALSE} otherwise.
##' @author Stefan Theussl
##' @export
is.L_constraint <- function( x ) {
  inherits( x, "L_constraint" )
}

## combining matrices (see 'rbind' in matrix.R, package relation)

##' Take a sequence of constraints (ROI objects) arguments and combine
##' by rows, i.e., putting several constraints together.
##'
##' The output type is determined from the highest type of the
##' components in the hierarchy NULL < \code{"L_constraint"} <
##' \code{"Q_constraint"} < \code{"F_constraint"}.
##'
##' @title Linear Constraints
##' @param ... constraints objects to be concatenated.
##' @param recursive logical. Currently ignored (enable compatibility
##' with \code{c()} operator).
##' @return an object of a class depending on the input which also
##' inherits from \code{"constraint"}. See \bold{Details}.
##' @author Stefan Theussl
##' @S3method rbind L_constraint
rbind.L_constraint <- function( ..., recursive = FALSE ){
  constraints <- lapply(list(...), as.L_constraint)
  L   <- lapply( constraints, function (x) as.simple_triplet_matrix(x$L) )
  dir <- lapply( constraints, function (x) as.character(x$dir) )
  rhs <- lapply( constraints, function (x) as.rhs(x$rhs) )
  L_constraint( L =   Reduce(function(x, y) rbind(x, y), L),
                dir = Reduce(function(x, y) c(x, y), dir),
                rhs = Reduce(function(x, y) c(x, y), rhs) )
}

## FIXME: connection to rbind documentation

##' @method c L_constraint
##' @S3method c L_constraint
c.L_constraint <- function( ..., recursive = FALSE )
    rbind( ..., recursive = recursive )

##' Get the number of constraints from a corresponding ROI object.
##'
##' @title Linear Constraints
##' @param x constraints object.
##' @return an integer.
##' @author Stefan Theussl
##' @method length L_constraint
##' @S3method length L_constraint
length.L_constraint <- function( x )
  x$n_L_constraints
## the linear term of the left hand side

as.L_term <- function( x, ... )
  UseMethod("as.L_term")

##' @S3method as.L_term numeric
as.L_term.numeric <- function( x, ... )
  as.simple_triplet_matrix( matrix(x, nrow = 1L) )

##' @S3method as.L_term matrix
as.L_term.matrix <- function( x, ... )
  as.simple_triplet_matrix(x)

##' @S3method as.L_term simple_triplet_matrix
as.L_term.simple_triplet_matrix <- function( x, ... )
  x


################################################################################
## Quadratic constraints (class 'Q_constraint')
## list of constraints of the form a'x + x'Qx ~ b

##' Quadratic constraints are typically of the form
##' \eqn{\frac{1}{2}x^{\top}Qx + c^{\top}x \leq b}. \eqn{A} is a
##' (sparse) matrix of coefficients to the objective variables \eqn{x}
##' of the quadratic part and \eqn{c} is the vector of coefficients of
##' the linear part of a given constraint. \eqn{b} is called the right
##' hand side of the constraints.
##'
##' @title Quadratic Constraints
##' @param Q a list of (sparse) matrices representing the quadratic
##' part of each constraint.
##' @param L a numeric vector of length \eqn{n} (a single constraint)
##' or a matrix of dimension \eqn{n \times m}, where \eqn{n} is the
##' number of objective variables and \eqn{m} is the number of
##' constraints. Matrices can be of class
##' \code{"simple_triplet_matrix"} to allow a sparse representation of
##' constraints.
##' @param dir a character vector with the directions of the
##' constraints. Each element must be one of \code{"<"}, \code{"<="},
##' \code{">"}, \code{">="}, \code{"=="} or \code{"!="}.
##' @param rhs a numeric vector with the right hand side of the
##' constraints.
##' @return an object of class \code{"Q_constraint"} which inherits
##' from \code{"constraint"}.
##' @author Stefan Theussl
##' @export
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

##' Coerces objects of type \code{"Q_constraint"}.
##'
##' Objects from the following classes can be coerced to
##' \code{"Q_constraint"}: \code{"list"}. The \code{"list"} must
##' contain four elements, a list of matrices \eqn{Q_m} representing
##' the quadratic part of \eqn{m} constraints, the matrix \eqn{A}
##' describing the linear part, the direction of the constraints, and
##' the right hand side.
##' @title Quadratic Constraints
##' @param x an R object.
##' @param ... further arguments passed to or from other methods
##' (currently ignored).
##' @return an object of class \code{"Q_constraint"} which inherits
##' from \code{"constraint"}.
##' @author Stefan Theussl
##' @export
as.Q_constraint <- function( x )
  UseMethod("as.Q_constraint")

##' S3method as.Q_constraint Q_constraint
as.Q_constraint.Q_constraint <-
  identity

##' S3method as.Q_constraint list
as.Q_constraint.list <- function( x ){
  names(x) <- c("Q", "L", "dir", "rhs")
  Q_constraint( Q = x$Q, L = x$L, dir = x$dir, rhs = x$rhs )
}

##' Tests if an object is interpretable as being of class \code{"Q_constraint"}.
##'
##' @title Quadratic Constraints
##' @param x object to be tested.
##' @return returns \code{TRUE} if its argument is of class
##' \code{"Q_constraint"} and \code{FALSE} otherwise.
##' @author Stefan Theussl
##' @export
is.Q_constraint <- function( x ) {
  inherits( x, "Q_constraint" )
}

## TODO: Q part not implemented
rbind.Q_constraint <- function( ..., recursive = FALSE ){
  constraints <- lapply(list(...), as.Q_constraint)


  L_constraint( L =   Reduce(function(x, y) rbind(x, y), lapply( constraints, function (x) as.simple_triplet_matrix(x$L) )),
                dir = Reduce(function(x, y) c(x, y), lapply( constraints, function (x) as.character(x$dir) )),
                rhs = Reduce(function(x, y) c(x, y), lapply( constraints, function (x) as.rhs(x$rhs) )) )
}

c.Q_constraint <- function( ..., recursive = FALSE )
    rbind( ..., recursive = recursive )

length.Q_constraint <- function(x)
  x$n_Q_constraints

## the quadratic term of the left hand side

as.Q_term <- function(x, ...)
  UseMethod( "as.Q_term" )

##' @S3method as.Q_term list
as.Q_term.list <- function( x )
  lapply( x, function(x) if( !is.null(x) ) as.simple_triplet_matrix(x) )

##' @S3method as.Q_term numeric
as.Q_term.numeric <- function( x )
  list( as.simple_triplet_matrix( matrix(x)) )

##' @S3method as.Q_term matrix
as.Q_term.matrix <- function( x )
  list( as.simple_triplet_matrix(x) )

##' @S3method as.Q_term simple_triplet_matrix
as.Q_term.simple_triplet_matrix <- function( x )
  list( x )

## combine, print, and summary methods

##summary.Q_constraint <- function(x){
##
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

## FIXME: F_constraint methods
as.F_constraint <- function(x, ...)
  UseMethod("as.F_constraint")

as.F_term <- function(x, ...)
  UseMethod( "as.F_term" )
length.F_constraint <- function(x)
  x$n_F_constraints

as.F_term.function <- function(x)
  list( x )

as.F_term.list <- function(x)
  lapply( x, as.function )


################################################################################
## constraint helper functions

as.rhs <- function(x, ...)
  UseMethod("as.rhs")

##' @S3method as.rhs numeric
as.rhs.numeric <- function( x, ... )
  x

##' @export
as.constraint <- function( x )
  UseMethod("as.constraint")

##' @method as.constraint NULL
##' @S3method as.constraint NULL
as.constraint.NULL <- identity


##' @S3method as.constraint L_constraint
as.constraint.L_constraint <-
  identity

##' @S3method as.constraint Q_constraint
as.constraint.Q_constraint <-
  identity

##' @S3method as.constraint F_constraint
as.constraint.F_constraint <-
  identity

##' @method print constraint
##' @S3method print constraint
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


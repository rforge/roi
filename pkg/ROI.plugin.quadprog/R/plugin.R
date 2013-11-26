## ROI plugin: quadprog
## based on quadprog package

## BASIC SOLVER METHOD
solve_QP <- function( x, control ) {
    ## quadprog does not support variable bounds per se, thus we add
    ## them as constraints
    x <- ROI:::as.no_V_bounds_OP( x )

    ## since ROI 0.1 the objective function is an STM, quadprog only supports 1 (dense) row
    L <- if( slam::is.simple_triplet_matrix(terms(objective(x))$L) ){
        stopifnot( dim(terms(objective(x))$L)[1] == 1L )
        as.numeric( as.matrix(terms(objective(x))$L) )
    } else {
        terms(objective(x))$L
    }

    ## quadprog needs an appropiately formated linear part of the objective function
    if( !length(L) )
        L <- double(length(objective(x)))
    stopifnot( length(L) == length(objective(x)) )

    ## solve the QP
    out <- .quadprog_solve_QP(Q   = terms(objective(x))$Q,
                              L   = L,
                              mat = constraints(x)$L,
                              dir = constraints(x)$dir,
                              rhs = constraints(x)$rhs,
                              max = x$maximum)
    ROI:::canonicalize_solution( solution = out$sol,
                                 optimum  = objective(x)(out$sol),
                                 status   = out$ierr,
                                 solver   = ROI:::get_solver_name(getPackageName()) )
}

## SOLVER SUBMETHODS
.quadprog_solve_QP <- function(Q, L, mat, dir, rhs, max) {

  ## Partially borrowed from the fPortfolio function '.rquadprog'
  ## Description:
  ##   Goldfarb and Idnani's quadprog solver function
  ## Note:
  ##   Requires to load contributed R package quadprog from which we
  ##   directly use the Fortran subroutine of the quadratic solver.

  ind_eq  <- which( dir == "==")
  ind_geq <- which( (dir == ">=") | (dir == ">") )
  ind_leq <- which( (dir == "<=") | (dir == "<") )
  meq <- length(ind_eq)

  ## everything except equaltity constraints have to be ">="
  ## FIXME: no replacement method for 'simple_triplet_matrix[i, ]<-'
  ##       thus, coercing to 'matrix'
  Amat <- as.matrix(mat)
  Amat[ ind_leq, ] <- -Amat[ ind_leq, ]
  bvec <- rhs
  bvec[ ind_leq ] <- -bvec[ ind_leq ]

  ## We have to sort constraints. The first meq constraints are
  ## equality constraints
  if( length(ind_eq) ) {
    Amat <- rbind( Amat[ ind_eq, ], Amat[ -ind_eq, ] )
    bvec <- c( bvec[ ind_eq ], bvec[ -ind_eq ] )
  }
  ## quadprog uses mat^T in the constraints
  Amat <- t(Amat)
  ## replace Inf with .Machine$double.xmax
  Amat[ is.infinite(Amat) & (Amat <= 0) ] <- -.Machine$double.xmax
  Amat[ is.infinite(Amat) & (Amat >= 0) ] <-  .Machine$double.xmax

  ## dvec in objective function according to direction of optimization
  dvec <- if( max )
    L
  else
    -L

  ## same with the quadratic term
  Dmat <- if( max )
    -as.matrix(Q)
  else
    as.matrix(Q)

  ## number objectives
  n_obj <- nrow(Dmat)
  ## number constraints
  n_constr <- ncol(Amat)

  r = min(n_obj, n_constr)
  work = rep(0, 2 * n_obj+ r * (r + 5)/2 + 2 * n_constr + 1)

  ## FIXME: do we need santiy checks here?

  # Optimize:
  .Fortran("qpgen2",
           as.double(Dmat),
           dvec = as.double(dvec),
           as.integer(n_obj),
           as.integer(n_obj),
           sol = double(n_obj),
           lagr = double(n_constr),
           crval = double(1),
           as.double(Amat),
           as.double(bvec),
           as.integer(n_obj),
           as.integer(n_constr),
           as.integer(meq),
           iact = integer(n_constr),
           nact = 0L,
           iter = integer(2L),
           work = as.double(work),
           ierr = 0L, NAOK = TRUE, PACKAGE = "quadprog")
}


## STATUS CODES
.add_status_codes <- function(){
    ## quadprog
    solver <- ROI:::get_solver_name( getPackageName() )
    ROI:::add_status_code_to_db(solver,
                                0L,
                                "OPTIMAL",
                                "Solution is optimal",
                                0L
                                )
    ROI:::add_status_code_to_db(solver,
                                1L,
                                "INCONSISTENT",
                                "Constraints are inconsistent, no solution."
                                )
    ROI:::add_status_code_to_db(solver,
                                2L,
                                "NOT_POSITIVE_DEFINITE",
                                "quadratic term in function is not positive definite."
                                )
    invisible(TRUE)
}

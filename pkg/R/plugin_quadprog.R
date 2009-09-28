## ROI plugin: CPLEX
## based on Rcplex interface

## SOLVER METHODS
.solve_LP.quadprog <- function( x, control ) {

}

.solve_QP.quadprog <- function( x, control ) {
  ## FIXME: currently we have no support for variable bounds
  if( length(x$bounds) )
    warning( "no bounds support yet" )
  out <- .quadprog_solve_QP(Q = terms(objective(x))$Q,
                            L = terms(objective(x))$L,
                            mat = constraints(x)$L,
                            dir = constraints(x)$dir,
                            rhs = constraints(x)$rhs,
                            max = x$maximum)
  class(out) <- c(class(x), class(out))
  .canonicalize_solution(out, x)
}

.quadprog_solve_QP <- function(Q, L, mat, dir, rhs, max) {

  ## Partially borrowed from the fPortfolio function '.rquadprog'
  ## Description:
  ##   Goldfarb and Idnani's quadprog solver function
  ## Note:
  ##   Requires to load contributed R package quadprog from which we directly use
  ##   the Fortran subroutine of the quadratic solver.

  ind_eq  <- which( dir == "==")
  ind_geq <- which( (dir == ">=") | (dir == ">") )
  ind_leq <- which( (dir == "<=") | (dir == "<") )
  meq <- length(ind_eq)

  ## everything except equaltity constraints have to be ">="
  ## FIXME: no replacement method for 'simple_triplet_matrix[i, ]<-'
  ##       thus, coercing to 'matrix'
  Amat <- as.matrix(mat)
  Amat[ ind_leq ] <- -Amat[ ind_leq ]
  bvec <- rhs
  bvec[ ind_leq ] <- -bvec[ ind_leq ]

  ## We have to sort constraints. The first meq constraints are
  ## equality constraints
  if( length(ind_eq) ) {
    Amat <- rbind( Amat[ ind_eq ], Amat[ -ind_eq ] )
    bvec <- c( bvec[ ind_eq ], bvec[ -ind_eq ] )
  }
  ## quadprog uses mat^T in the constraints
  Amat <- t(Amat)

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
  require("quadprog")
  .Fortran("qpgen2",
           as.double(Dmat),
           dvec = as.double(dvec),
           as.integer(n_obj),
           as.integer(n_obj),
           sol = double(n_obj),
           crval = double(1),
           as.double(Amat),
           as.double(bvec),
           as.integer(n_obj),
           as.integer(n_constr),
           as.integer(meq),
           iact = rep(0L, n_constr),
           nact = 0L,
           iter = rep(0L, 2L),
           work = as.double(work),
           ierr = 0L,
           PACKAGE = "quadprog")  
}

## CANONICALIZER
.canonicalize_solution.quadprog <- function(out, x){
  solution <- out$sol
  objval <- objective(x)(solution)
  status <- .canonicalize_status(out$ierr, class(out)[1])
    .make_MIP_solution(solution, objval, status)
}

## STATUS CODES
.add_quadprog_status_codes <- function(){
  ## quadprog
  add_status_code_to_db("quadprog", 
                        0L,
                        "OPTIMAL",
                        "Solution is optimal",
                        0L
                        )
  add_status_code_to_db("quadprog", 
                        1L,
                        "INCONSISTENT",
                        "Constraints are inconsistent, no solution."
                        )
  add_status_code_to_db("quadprog", 
                        2L,
                        "NOT_POSITIVE_DEFINITE",
                        "quadratic term in function is not positive definite."
                        )  
  invisible(TRUE)
}


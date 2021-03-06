## ROI plugin: nlminb
## based on MySolver Template

## SOLVER METHODS

## we need for each problem class a separate solver method

.solve_QP_nlminb <- function( x, control ) {
  ## if needed, add constraints made from variable bounds
  ##if( length(bounds(x)) )
  ##  constraints(x) <- rbind(constraints(x),
  ##                         .make_box_constraints_from_bounds(bounds(x),
  ##                                     dim(terms(objective(x))$Q)[1]) )

  ## solve the QP
  ## adjust arguments depending on problem class
  out <- .nlminb_solve_QP(Q = terms(objective(x))$Q,
                          L = terms(objective(x))$L,
                          mat = constraints(x)$L,
                          dir = constraints(x)$dir,
                          rhs = constraints(x)$rhs,
                          bounds = bounds(x),
                          max = x$maximum,
                          control = control
                          )
  class(out) <- c(class(x), class(out))
  .canonicalize_solution(out, x)
}

.nlminb_solve_QP <- function(Q, L, mat, dir, rhs, max, control = list()) {

    solver <- "nlminb"

    # nlminb does not directly support constraints
    # we need to translate Ax ~ b constraints to lower, upper bounds

    A <- solve(t(mat))
    n_obj <- ifelse( !is.null(Q),
                    dim(Q)[1],
                    length(L) )

    ## start
    start <- as.numeric( control$start )
    if( !length(start) )
        start <- mat %*% rep(1/n_obj, n_obj)
    stopifnot( length(start) == n_obj)

    lower <- rhs
    upper <- c(Inf, Inf, Inf)

    ## possibly transformed objective function
    foo <- function(x, L, mat, A, Q) {
        X = as.vector(A %*% x)
        Objective = - t(L) %*% X + 0.5 * ( t(X) %*% Q %*% X )
        Objective[[1]]
    }
    out <- nlminb(start, foo, lower = lower, upper = upper,
                  L = L, mat = mat, A = A, Q = Q)
    out$solution <- as.vector(A %*% out$par)

    # Return Value:
    canonicalize_solution( solution = out$solution,
                           optimum = objective(x)(out$solution),
                           status = out$convergence,
                           solver = solver )
}


## STATUS CODES

.add_nlminb_status_codes <- function(){
  ## add all status codes generated by the solver to db

  ## Two examples are listed here:
  add_status_code_to_db("nlminb",
                        0L,
                        "CONVERGENCE",
                        "Solution is optimal",
                        0L
                        )
  add_status_code_to_db("nlminb",
                        1L,
                        "NON_CONVERGENCE",
                        "No solution."
                        )
  invisible(TRUE)
}


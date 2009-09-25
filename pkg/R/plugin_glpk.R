## ROI plugin: GLPK

## SOLVER METHODS
.solve_LP.glpk <- function( x, control ) {
    out <- Rglpk::Rglpk_solve_LP(terms(objective(x)),
                                 constraints(x)$L,
                                 constraints(x)$dir,
                                 constraints(x)$rhs,
                                 bounds = x$bounds,
                                 max = x$maximum)
    class(out) <- c(class(x), class(out))
    .canonicalize_solution(out, x)
}


.solve_MILP.glpk <- function( x, control ) {
    out <- Rglpk::Rglpk_solve_LP(terms(objective(x)),
                                 constraints(x)$L,
                                 constraints(x)$dir,
                                 constraints(x)$rhs,
                                 bounds = x$bounds,
                                 types = x$types,
                                 max = x$maximum)
    class(out) <- c(class(x), class(out))
    .canonicalize_solution(out, x)
}

## CANONICALIZER
.canonicalize_solution.glpk <- function(out, x)
{
    status <- .canonicalize_status(out$status, class(out)[1])
    .make_MIP_solution(out$solution, out$optimum, status)
}

## STATUS CODES
.add_glpk_status_codes <- function(){
  ## GLPK
  ## from GLPK 4.34 reference manual and glpk.h (symbol, code, message)
  ## FIXME: change in solver interface, canonicalization now done in ROI
  add_status_code_to_db("glpk", 
                        0L,
                        "GLP_OPT",
                        "(DEPRECATED) Solution is optimal. Compatibility status code will be removed in Rglpk soon.",
                        0L
                        )
  add_status_code_to_db("glpk", 
                        1L,
                        "GLP_UNDEF",
                        "Solution is undefined."
                        )
  add_status_code_to_db("glpk", 
                        2L,
                        "GLP_FEAS",
                        "Solution is feasible."
                        )
  add_status_code_to_db("glpk", 
                        3L,
                        "GLP_INFEAS",
                        "Solution is infeasible."
                        )
  add_status_code_to_db("glpk", 
                        4L,
                        "GLP_NOFEAS",
                        "No feasible solution exists."
                        )
  add_status_code_to_db("glpk", 
                        5L,
                        "GLP_OPT",
                        "Solution is optimal.",
                        0L
                        )
  add_status_code_to_db("glpk", 
                        6L,
                        "GLP_UNBND",
                        "Solution is unbounded."
                        )
  invisible(TRUE)
}

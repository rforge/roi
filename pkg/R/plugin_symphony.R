## ROI plugin: SYMPHONY

## SOLVER METHODS
.solve_LP.symphony <- function(x, control) {
    out <- Rsymphony::Rsymphony_solve_LP(terms(objective(x)),
                                         constraints(x)$L,
                                         constraints(x)$dir,
                                         constraints(x)$rhs,
                                         bounds = x$bounds,
                                         max = x$maximum)
    class(out) <- c(class(x), class(out))
    .canonicalize_solution(out, x)
}

.solve_MILP.symphony <- function(x, control) {
    out <- Rsymphony::Rsymphony_solve_LP(terms(objective(x)),
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
.canonicalize_solution.symphony <- function(out, x) {
    status <- .canonicalize_status(out$status, class(out)[1])
    .make_MIP_solution(out$solution, out$opjval, status)
}

## STATUS CODES
.add_symphony_status_codes <- function( ) {

  ## SYMPHONY
  ## SYMPHONY 5.5.10 (no reference found yet)
  ## FIXME: better description of status in message

  ## FIXME: change in solver interface, canonicalization now done in ROI
  add_status_code_to_db("symphony", 
                        0L,
                        "TM_OPTIMAL_SOLUTION_FOUND",
                        "(DEPRECATED) Solution is optimal. Compatibility status code will be removed in Rsymphony soon.",
                        0L
                        )
  add_status_code_to_db("symphony", 
                        225L,
                        "TM_NO_PROBLEM",
                        "TM_NO_PROBLEM"
                        )
  add_status_code_to_db("symphony", 
                        226L,
                        "TM_NO_SOLUTION",
                        "TM_NO_SOLUTION"
                        )
  add_status_code_to_db("symphony", 
                        227L,
                        "TM_OPTIMAL_SOLUTION_FOUND",
                        "Solution is optimal.",
                        0L
                        )
  add_status_code_to_db("symphony", 
                        228L,
                        "TM_TIME_LIMIT_EXCEEDED",
                        "TM_TIME_LIMIT_EXCEEDED"
                        )
  add_status_code_to_db("symphony", 
                        229L,
                        "TM_NODE_LIMIT_EXCEEDED",
                        "TM_NODE_LIMIT_EXCEEDED"
                        )
  add_status_code_to_db("symphony", 
                        230L,
                        "TM_TARGET_GAP_ACHIEVED",
                        "TM_TARGET_GAP_ACHIEVED"
                        )
  add_status_code_to_db("symphony", 
                        231L,
                        "TM_FOUND_FIRST_FEASIBLE",
                        "TM_FOUND_FIRST_FEASIBLE"
                        )
  add_status_code_to_db("symphony", 
                        232L,
                        "TM_FINISHED",
                        "TM_FINISHED"
                        )
  add_status_code_to_db("symphony", 
                        233L,
                        "TM_UNFINISHED",
                        "TM_UNFINISHED"
                        )
  add_status_code_to_db("symphony", 
                        240L,
                        "TM_FEASIBLE_SOLUTION_FOUND",
                        "TM_FEASIBLE_SOLUTION_FOUND"
                        )
  add_status_code_to_db("symphony", 
                        235L,
                        "TM_SIGNAL_CAUGHT",
                        "TM_SIGNAL_CAUGHT"
                        )
  add_status_code_to_db("symphony", 
                        -251L,
                        "TM_ERROR__NO_BRANCHING_CANDIDATE",
                        "TM_ERROR__NO_BRANCHING_CANDIDATE"
                        )
  add_status_code_to_db("symphony", 
                        -252L,
                        "TM_ERROR__ILLEGAL_RETURN_CODE",
                        "TM_ERROR__ILLEGAL_RETURN_CODE"
                        )
  add_status_code_to_db("symphony", 
                        -253L,
                        "TM_ERROR__NUMERICAL_INSTABILITY",
                        "TM_ERROR__NUMERICAL_INSTABILITY"
                        )
  add_status_code_to_db("symphony", 
                        -254L,
                        "TM_ERROR__COMM_ERROR",
                        "TM_ERROR__COMM_ERROR"
                        )
  add_status_code_to_db("symphony", 
                        -275L,
                        "TM_ERROR__USER",
                        "TM_ERROR__USER"
                       )
  invisible(TRUE)
}

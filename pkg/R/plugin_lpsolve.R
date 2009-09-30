## ROI plugin: lp_solve
## based on lpSolve interface

## FIXME: we should really switch to lpsolveAPI since lpSolve
##        1) returns wrong results, especially for large scale or
##           complex proplems
##        2) does support neither, -Inf, Inf nor any reasonable
##           replacement like .Machine$double.xmax
##        3) solutions only take nonnegative values 

## SOLVER METHODS
.solve_LP.lpsolve <- function( x, control ) {
  .solve_MILP.lpsolve( x, control )
}

.solve_MILP.lpsolve <- function( x, control ) {
    ## Currently, no direct support for bounds.
    ## <FIXME>
    ## Should rewrite the given bounds into additional constraints.
    ## partially solved -> 0, Inf box constraints available
    if(!is.null(x$bounds))
      x <- .make_box_constraints_from_bounds_in_MIP(x, negative = FALSE)
    ## </FIXME>

    types <- .expand_types(x$types, length(terms(objective(x))[["L"]]))

    ## Version 5.6.1 of lpSolve has added sparse matrix support via
    ## formal 'dense.const' as well as binary variable types.
    mat <- constraints(x)$L
    ## FIXME: problem constructors stores matrices always as
    ## simple_triplet_matrix!
    out <- if(is.simple_triplet_matrix(mat)) {
        ## In the sparse case, lpSolve currently (2008-11-22) checks
        ## that every constraint is used in the sense that each row has
        ## at least one entry.  So if for some reason this is not the
        ## case, let us add one zero entry for such rows (note that we
        ## cannot simply drop them as the corresponding constraint may
        ## be violated).
        ind <- which(tabulate(mat$i, mat$nrow) == 0)
        if(len <- length(ind)) {
            mat$i <- c(mat$i, ind)
            mat$j <- c(mat$j, rep.int(1L, len))
            mat$v <- c(mat$v, rep.int(0, len))
        }
        lpSolve::lp(if(x$maximum) "max" else "min",
                    terms(objective(x))[["L"]],
                    const.dir = constraints(x)$dir,
                    const.rhs = constraints(x)$rhs,
                    int.vec = which(types == "I"),
                    binary.vec = which(types == "B"),
                    dense.const = cbind(mat$i, mat$j, mat$v))
    } else {
        lpSolve::lp(if(x$maximum) "max" else "min",
                    terms(objective(x))[["L"]],
                    as.matrix(mat),
                    constraints(x)$dir,
                    constraints(x)$rhs,
                    int.vec = which(types == "I"),
                    binary.vec = which(types == "B"))
    }
    class(out) <- c(class(x), class(out))
    .canonicalize_solution(out, x)
}

## CANONICALIZER
.canonicalize_solution.lpsolve <- function(out, x)
{
    solution <- out$solution
    status <- .canonicalize_status(out$status, class(out)[1])
    objval <- if(status$code == 0L)
      objective(x)(solution)
    else
        out$objval
    .make_MIP_solution(solution, objval, status)
}

## STATUS CODES
.add_lpsolve_status_codes <- function(){
  ## lp_solve
  ## from lp_solve.h
  ## FIXME: currently only repetition of symbol in message
  
  add_status_code_to_db("lpsolve", 
                        -5L,
                        "UNKNOWNERROR", 
                        "UNKNOWNERROR"
                        )
  add_status_code_to_db("lpsolve", 
                        -4L,
                        "DATAIGNORED", 
                        "DATAIGNORED"
                        )
  add_status_code_to_db("lpsolve", 
                        -3L,
                        "NOBFP", 
                        "NOBFP"
                        )
  add_status_code_to_db("lpsolve", 
                        -2L,
                        "NOMEMORY", 
                        "NOMEMORY"
                        )
  add_status_code_to_db("lpsolve", 
                        -1L,
                        "NOTRUN", 
                        "NOTRUN"
                        )
  add_status_code_to_db("lpsolve", 
                        0L,
                        "OPTIMAL", 
                        "OPTIMAL",
                        0L
                        )
  add_status_code_to_db("lpsolve", 
                        1L,
                        "SUBOPTIMAL", 
                        "SUBOPTIMAL"
                        )
  add_status_code_to_db("lpsolve", 
                        2L,
                        "INFEASIBLE", 
                        "INFEASIBLE"
                        )
  add_status_code_to_db("lpsolve", 
                        3L,
                        "UNBOUNDED", 
                        "UNBOUNDED"
                        )
  add_status_code_to_db("lpsolve", 
                        4L,
                        "DEGENERATE", 
                        "DEGENERATE"
                        )
  add_status_code_to_db("lpsolve", 
                        5L,
                        "NUMFAILURE", 
                        "NUMFAILURE"
                        )
  add_status_code_to_db("lpsolve", 
                        6L,
                        "USERABORT", 
                        "USERABORT"
                        )
  add_status_code_to_db("lpsolve", 
                        7L,
                        "TIMEOUT", 
                        "TIMEOUT"
                        )
  add_status_code_to_db("lpsolve", 
                        8L,
                        "RUNNING", 
                        "RUNNING"
                        )
  add_status_code_to_db("lpsolve", 
                        9L,
                        "PRESOLVED", 
                        "PRESOLVED"
                        )
  invisible(TRUE)
}

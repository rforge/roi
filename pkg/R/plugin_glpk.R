## ROI plugin: GLPK

.solve_MILP_via_glpk <-
function(x)
{
    out <- Rglpk::Rglpk_solve_LP(x$objective,
                                 x$constraints$mat,
                                 x$constraints$dir,
                                 x$constraints$rhs,
                                 bounds = x$bounds,
                                 types = x$types,
                                 max = x$maximum)
    .make_MIP_solution(out$solution, out$optimum, out$status)
}

### ** lp_solve

.solve_MILP_via_lpsolve <-
function(x)
{
    ## Currently, no direct support for bounds.
    ## <FIXME>
    ## Should rewrite the given bounds into additional constraints.
    if(!is.null(x$bounds))
        stop("Solver currently does not support variable bounds.")
    ## </FIXME>

    types <- .expand_types(x$types, length(x$objective))

    ## Version 5.6.1 of lpSolve has added sparse matrix support via
    ## formal 'dense.const' as well as binary variable types.
    mat <- x$constraints$mat
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
                    x$objective,
                    const.dir = x$constraints$dir,
                    const.rhs = x$constraints$rhs,
                    int.vec = which(types == "I"),
                    binary.vec = which(types == "B"),
                    dense.const = cbind(mat$i, mat$j, mat$v))
    } else {
        lpSolve::lp(if(x$maximum) "max" else "min",
                    x$objective,
                    as.matrix(mat),
                    x$constraints$dir,
                    x$constraints$rhs,
                    int.vec = which(types == "I"),
                    binary.vec = which(types == "B"))
    }
    status_db <-
        ## Solver status values from lp_lib.h:
       c("UNKNOWNERROR" = -5L,
         "DATAIGNORED" = -4L,
         "NOBFP" = -3L,
         "NOMEMORY" = -2L,
         "NOTRUN" = -1L,
         "OPTIMAL" = 0L,
         "SUBOPTIMAL" = 1L,
         "INFEASIBLE" = 2L,
         "UNBOUNDED" = 3L,
         "DEGENERATE" = 4L,
         "NUMFAILURE" = 5L,
         "USERABORT" = 6L,
         "TIMEOUT" = 7L,
         "RUNNING" = 8L,
         "PRESOLVED" = 9L)
    status <- status_db[match(out$status, status_db)]
    solution <- out$solution
    objval <- if(status == 0L)
        sum(solution * out$objective)
    else
        out$objval
    .make_MIP_solution(solution, objval, status)
}

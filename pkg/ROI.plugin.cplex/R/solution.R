## ROI.plugin.cplex: status codes
## Description: provides canonicalization of solution object

## CANONICALIZER
.canonicalize_solution.cplex <- function(out, x)
{
    solution <- out$xopt
    ## For the time being ...
    ## Since Rcplex 0.1-4 integers are rounded (via control argument
    ## 'round' which we set accordingly when calling Rcplex()) but no
    ## new optimal solution based on these values is calculated.  Hence,
    ## we no longer round ourselves, but recompute objval.
    objval <- objective(x)(solution)
    status <- .canonicalize_status(out$status, class(out)[1])
    .make_MIP_solution(solution, objval, status)
}

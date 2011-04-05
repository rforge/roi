.solve_empty_MIP <-
function(x)
{
    ## Check whether constraints are satisfied (interpreting each lhs as
    ## empty sum with value 0):
    constraints <- split(constraints(x)$rhs, constraints(x)$dir)
    if(all(unlist(Map(function(dir, rhs) get(dir)(0, rhs),
                      names(constraints), constraints))))
        .make_MIP_solution(double(), 0, 0L)
    else
        .make_MIP_solution(double(), NA_real_, 2L)
}

.make_MIP_solution <-
function(solution, objval, status, ...)
    structure(list(solution = solution,
                   objval = objval,
                   status = status, ...),
              class = "MIP_solution")


.canonicalize_status <- function(status, solver){
  msg <- get_status_message_from_db(solver, status)
  list(code = msg$roi_code, msg = msg)
}

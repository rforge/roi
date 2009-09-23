## ROI plugin: SYMPHONY

.solve_MILP_via_symphony <-
function(x)
{
    out <- Rsymphony::Rsymphony_solve_LP(x$objective,
                                         x$constraints$mat,
                                         x$constraints$dir,
                                         x$constraints$rhs,
                                         bounds = x$bounds,
                                         types = x$types,
                                         max = x$maximum)
    .make_MIP_solution(out$solution, out$objval, out$status)
}

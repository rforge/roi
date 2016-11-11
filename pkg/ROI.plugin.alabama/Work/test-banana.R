q("no")
R

library( testthat )
library( ROI )

ROI_registered_solvers()
    
control <- list()
## initial values
control$start <- c( -1.2, 1 )

## Rosenbrock Banana function and gradient in separate functions
f_objective <- function(x) 100 * (x[2] - x[1] * x[1])^2 + (1 - x[1])^2
f_gradient <- function(x) {
    c( -400 *  x[1] * (x[2] - x[1] * x[1]) - 2 * (1 - x[1]),
        200 * (x[2] -  x[1] * x[1]) )
}
x <- OP( objective = F_objective(F=f_objective, n=2L, G=f_gradient),
         bounds = V_bound(1:2, 1:2, rep(-Inf, 2), rep(Inf, 2)) )

## Solve Rosenbrock Banana function.
res <- ROI_solve( x, solver="solnp", control )
## str(res)
res
solution(res)


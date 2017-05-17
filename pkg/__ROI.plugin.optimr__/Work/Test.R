q("no")
R

library(ROI)

## Rosenbrock Banana objective function
## solution: c(1, 1)
f <- function(x) {
    return( 100 * (x[2] - x[1] * x[1])^2 + (1 - x[1])^2 )
}

f.grad <- function(x) {
    return( c( -400 * x[1] * (x[2] - x[1] * x[1]) - 2 * (1 - x[1]),
               200 * (x[2] - x[1] * x[1])) )
}

## lower and upper bounds
lb <- c( -3, -3 )
ub <- c(  3,  3 )

VB <- function(lower, upper) {
    li <- which(lower != 0)
    ui <- which(upper != Inf)
    return(V_bound(li=li, ui=ui, lb=lower[li], ub=upper[ui], nobj=length(lower)))
}


x <- OP(F_objective(f, 2L, f.grad), bounds=VB(lb, ub))
control <- list(start = c(-1.2, 1), method = "L-BFGS-B")
# Solve Rosenbrock Banana function.
res <- ROI_solve(x, solver="optimr", control)
res
solution(res)


x <- OP(F_objective(f, 2L), bounds=VB(lb, ub))
control <- list(start = c(-1.2, 1), method = "L-BFGS-B")
# Solve Rosenbrock Banana function.
res <- ROI_solve(x, solver="optimr", control)
res
solution(res)


x <- OP(F_objective(f, 2L, f.grad), bounds=VB(lb, ub))
control <- list(start = c(-1.2, 1))
# Solve Rosenbrock Banana function.
res <- ROI_solve(x, solver="optimr", control)
res
attributes(res)$meta
solution(res)
solution(res, "msg")


str(res)

res$message$message


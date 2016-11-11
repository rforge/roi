q("no")
R


library(DEoptim)

Rosenbrock <- function(x){
  x1 <- x[1]
  x2 <- x[2]
  100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}

lower <- c(-10,-10)
upper <- -lower
## run DEoptim and set a seed first for replicability
set.seed(1234)
z <- DEoptim(Rosenbrock, lower, upper)
str(z)
z$optim$bestval
sol <- z$optim$bestmem
sol



library(ROI)

## Rosenbrock Banana objective function
## solution: c(1, 1)
f <- function(x) {
    return( 100 * (x[2] - x[1] * x[1])^2 + (1 - x[1])^2 )
}

## lower and upper bounds
lb <- c( -3, -3 )
ub <- c(  3,  3 )

VB <- function(lower, upper) {
    li <- which(lower != 0)
    ui <- which(upper != Inf)
    return(V_bound(li=li, ui=ui, lb=lower[li], ub=upper[ui], nobj=length(lower)))
}

x <- OP(F_objective(f, 2L, names=c("x_1", "x_2")), bounds=VB(lb, ub))

setNames(sol, terms(objective(x))$names)
setNames(sol, terms(objective(x))$Tname)


control <- list(start = c(-1.2, 1), method = "L-BFGS-B")
# Solve Rosenbrock Banana function.
res <- ROI_solve(x, solver="optimx", control)
res
solution(res)


x <- OP(F_objective(f, 2L), bounds=VB(lb, ub))
control <- list(start = c(-1.2, 1), method = "L-BFGS-B")
# Solve Rosenbrock Banana function.
res <- ROI_solve(x, solver="optimx", control)
res
solution(res)


x <- OP(F_objective(f, 2L, f.grad), bounds=VB(lb, ub))
control <- list(start = c(-1.2, 1))
# Solve Rosenbrock Banana function.
res <- ROI_solve(x, solver="optimx", control)
res
attributes(res)$meta
solution(res)
solution(res, "msg")

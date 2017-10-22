q("no")
Rdevel

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


Sys.setenv("ROI_LOAD_PLUGINS" = FALSE)
library(ROI)
library(ROI.plugin.deoptim)

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

control <- list(start = c(-1.2, 1))
# Solve Rosenbrock Banana function.
res <- ROI_solve(x, solver="deoptim", control)
res
solution(res)
str(res)
res$message$optim


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




##
## f(x) = x1*x4*(x1 + x2 + x3) + x3
##
f_objective <- function(x) x[1]*x[4]*(x[1] + x[2] + x[3]) + x[3]
f_gradient <- function(x) c( x[1] * x[4] + x[4] * (x[1] + x[2] + x[3]),
                             x[1] * x[4],
                             x[1] * x[4] + 1.0,
                             x[1] * (x[1] + x[2] + x[3]) )

## Inequality constraints.
g_leq_constraints <- function(x) c( 25 - x[1] * x[2] * x[3] * x[4] )

g_leq_jacobian <- function(x) c( -x[2] * x[3] * x[4],
                                 -x[1] * x[3] * x[4],
                                 -x[1] * x[2] * x[4],
                                 -x[1] * x[2] * x[3] )

## Equality constraints.
h_eq_constraints <- function(x) x[1]^2 + x[2]^2 + x[3]^2 + x[4]^2 - 40

h_eq_jacobian <- function(x) c( 2.0 * x[1],
                                2.0 * x[2],
                                2.0 * x[3],
                                2.0 * x[4] )

## Optimal solution.
solution.opt <- c(1.00000000, 4.74299963, 3.82114998, 1.37940829)

control <- list( start = c( 1, 5, 5, 1 ) )

## Solve using NLOPT_LD_MMA with gradient information supplied in separate function
x <- OP(objective = F_objective(F=f_objective, n=4L, G=f_gradient), 
        constraints = F_constraint(F=c(g_leq_constraints, h_eq_constraints), 
                                   dir=c("<=", "=="), rhs=c(0, 0),
                                   J=c(g_leq_jacobian, h_eq_jacobian)),
        bounds = V_bound(li=1:4, ui=1:4, lb=rep.int(1, 4), ub=rep.int(5, 4)) )

z <- ROI_solve(x, "deoptim", control, maxiter = 10000L)
solution(z, force = TRUE)
solution(z, force = TRUE) - solution.opt

z <- ROI_solve(x, "deoptim", dry_run = TRUE, maxiter = 10000L)
s <- eval(z)
s$convergence


##
##
##
V_bound(li=1:4, ui=1:4, lb=rep.int(1, 4), ub=rep.int(5, 4))


## TODO
q("no")
Rdevel

library(ROI)

mat <- matrix(c(3, 4, 2,
                2, 1, 2,
                1, 3, 2), nrow=3, byrow=TRUE)
lp <- OP(objective = c(2, 4, 3),
         constraints = L_constraint(L = mat,
                                    dir = c("<=", "<=", "<="),
                                    rhs = c(60, 40, 80)),
         maximum = TRUE)

opt <- ROI_solve(lp)
opt
opt$solution

f0 <- function(x) -as.numeric(c(2, 4, 3) %*% x)
grad_f0 <- function(x) -c(2, 4, 3)
g0 <- function(x) as.numeric(mat %*% x) - c(60, 40, 80)
grad_g0 <- function(x) mat

obj <- F_objective(f0, 3, grad_f0)
con <- F_constraint(g0, dir="<=", rhs=0, J=grad_g0)
op <- OP(objective = obj, constraints = con, maximum = FALSE)
control <- list(algorithm = "NLOPT_LD_MMA", start=c(1, 7, 2))
ROI_solve(op, solver="nloptr", control=control)$solution

control <- list(method = "NLOPT_LD_MMA", start=c(1, 7, 2))
ROI_solve(op, solver="nloptr", control=control)$solution


f0 <- function(x) as.numeric(c(2, 4, 3) %*% x)
grad_f0 <- function(x) c(2, 4, 3)
g0 <- function(x) as.numeric(mat %*% x) - c(60, 40, 80)
grad_g0 <- function(x) mat

obj <- F_objective(f0, 3, grad_f0)
con <- F_constraint(g0, dir="<=", rhs=0, J=grad_g0)
op <- OP(objective = obj, constraints = con, maximum = TRUE)
control <- list(algorithm = "NLOPT_LD_MMA", start=c(1, 7, 2))
ROI_solve(op, solver="nloptr", control=control)$solution

## -------------------------------------
## Convert 
## -------------------------------------
lc <- L_constraint(L = mat, dir = c("<=", "<=", "<="), rhs = c(60, 40, 80))
lp <- OP(objective = -c(2, 4, 3),
         constraints =,
         maximum = FALSE)

obj <- as.F_objective(lp$objective)

fun <- function(i) {
    g <- as.matrix(tmp[i,])
    function(x) as.numeric(g %*% x)
}
tmp <- lc$L
F <- lapply(seq_len(NROW(tmp)), fun)
rm(tmp)
F[[1]](c(1, 1, 1))
lapply(F, function(g) g(c(1, 1, 1)))

FF <- function(x) {
    unlist(lapply(F, function(g) g(x)))
}

fcon <- F_constraint(F, dir = c("<=", "<=", "<="), rhs = c(60, 40, 80))
rhs <- c(60, 40, 80)
FF <- function(x) {
    unlist(mapply(function(g, r) g(x) - r, F, rhs))
}
FF(c(1, 1, 1))

## Des ist ned so anfoch denn wie des design jetzt ist, würde es bedeuten 
## das der Solver eine Liste von Funktionen verkraften muss.
library(nloptr)

mat <- matrix(c(3, 4, 2,
                2, 1, 2,
                1, 3, 2), nrow=3, byrow=TRUE)
mat
lp <- OP(objective = c(2, 4, 3),
         constraints = L_constraint(L = mat,
                                    dir = c("<=", "<=", "<="),
                                    rhs = c(60, 40, 80)),
         maximum = TRUE)


f0 <- function(x) -as.numeric(c(2, 4, 3) %*% x)
grad_f0 <- function(x) -c(2, 4, 3)
g0 <- function(x) as.numeric(mat %*% x) - c(60, 40, 80)
grad_g0 <- function(x) mat
    
g0()

s <- nloptr( x0=c(1, 6, 2), eval_f=f0, eval_grad_f=grad_f0, 
             lb = c(0, 0, 0), ub = c(Inf, Inf, Inf),
             eval_g_ineq = g0, eval_jac_g_ineq = grad_g0, 
             opts = list("algorithm" = "NLOPT_LD_MMA", xtol_abs = 1.0e-7))

s$solution
str(s)

args(check.derivatives)
check.derivatives(.x=c(0,0,0), func=f0, func_grad=grad_f0)



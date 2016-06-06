q("no")
Rdevel

##library(lineprof)
tmp <- tempfile()
Rprof(tmp)
library(ROI)
Rprof(NULL)

summaryRprof(tmp)

library(ROI)
library(slam)

source("/home/florian/work/Optimization/ROI/ROI_R-Forge/pkg/ROI.plugin.nloptr/Work/functions.R")

mat <- matrix(c(3, 4, 2, 2, 1, 2, 1, 3, 2), nrow=3, byrow=TRUE)
lo <- L_objective(-c(2, 4, 3))
lc <- L_constraint(L = mat, dir = c("<=", "<=", "<="), rhs = c(60, 40, 80))
lp <- OP(objective = lo, constraints = lc, maximum = FALSE)
ROI_solve(lp)$objval
ROI_solve(lp)$solution


flo <- as.F_objective(lo)
flc <- as.F_constraint_L(lc)
flp <- OP(objective = flo, constraints = flc, maximum = FALSE)
x <- flp

flo$F(c(1, 1, 1))
flo$G(c(1, 1, 1))

control <- list(algorithm="NLOPT_LD_MMA", x0=c(1, 7, 2))
ROI_solve(flp, control=control)$objval
ROI_solve(flp, control=control)$solution


f0 <- function(x) -as.numeric(c(2, 4, 3) %*% x)
grad_f0 <- function(x) -c(2, 4, 3)
g0 <- function(x) as.numeric(mat %*% x) - c(60, 40, 80)
grad_g0 <- function(x) mat

obj <- F_objective(f0, 3, grad_f0)
con <- F_constraint(g0, dir="<=", rhs=0, J=grad_g0)
op <- OP(objective = obj, constraints = con, maximum = FALSE)
control <- list(algorithm = "NLOPT_LD_MMA", start=c(1, 7, 2))
ROI_solve(op, solver="nloptr", control=control)$objval
ROI_solve(op, solver="nloptr", control=control)$solution

f0(c(1, 1, 1))
objective(x)(c(1, 1, 1))
grad_f0(c(1, 1, 1))
x$objective$G(c(1, 1, 1))
g0(c(1, 1, 1))
eval_g_ineq(c(1, 1, 1))
environment(eval_g_ineq)$g
eval_jac_g_ineq(c(1, 1, 1))
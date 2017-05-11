q("no")
Rdevel

library(slam)
library(ROI)

x <- OP(obj = c(1, 1),
        con = L_constraint(c(1, 1), "==", 1),
        typ = c("B", "B"))

so <- ROI_solve(x, solver = "msbinlp", method = "glpk", 
                nsol_max = 2L)
so
solution(so)

so <- ROI_solve(x, solver = "ecos", control = list(nsol_max = 2L))
so

solution(so)

so <- ROI_solve(x, solver = "lpsolve", control = list(nsol_max = 2L))
so
class(so)

solution(so)

ROI:::get_solver_controls_from_db("lpsolve")

attach(getNamespace("ROI"))


q("no")
Rdevel

library("ROI")
library("lpSolveAPI")

x <- OP(obj = c(1, 1),
        con = L_constraint(c(1, 1), "==", 1),
        typ = c("B", "B"))

control <- list(nsol_max = 3L)
so <- ROI_solve(x, solver = "lpsolve", control = control)
so

solution(so)


solu <- solution(so, "msg")
solu

str(.ROI_plugin_canonicalize_solution(2, 3, 0L, "lpsolve"))
traceback()

is_element(c(0, 1), solu)

z <- ROI_solve(x, solver = "lpsolve", dry_run = TRUE)


lp <- as.list(z)[[2]]


library(lpSolveAPI)

lp.control(lp, bb.rule = c("range", "autoorder"))
lp.control(lp, improve = "solution")
lp.control(lp)$mip.gap
lp.control(lp, mip.gap = c(1e-3, 1e-3))
lp.control(lp)$bb.depthlimit
lp.control(lp, bb.depthlimit = 0)
lp.control(lp)$break.at.first
lp.control(lp, break.at.first = 0L)
solve(lp)

get.solutioncount(lp)
get.total.nodes(lp)



##
## Mosek
##
library(Rmosek)
z <- ROI_solve(x, solver = "mosek", dry_run = TRUE)
lp_mosek <- as.list(z)[[2]]
lp_mosek
eval(z)

## -----------------------------------------------------------------------------
##
## Pruning Moves [Matteo Fischetti, Domenico Salvagnin] (2009)
##
## -----------------------------------------------------------------------------
## min   - x_1 - x_2 - x_3 - x_4 - 99 x_5
## s.t.  x_1 + x_2 <= 1
##       x_3 + x_4 <= 1
##       x_4 + x_5 <= 1
##       x_i in {0, 1}
x <- OP()
objective(x) <- L_objective(c(-1, -1, -1, -1, -99))
mat <- simple_triplet_matrix(rep(1:3, 2), c(c(1, 3, 4), c(2, 4, 5)), rep(1, 6))
as.matrix(mat)
constraints(x) <- L_constraint(mat, dir = leq(3), rhs = rep.int(1, 3))
types(x) <- rep("B", length(x))

control <- list(nsol_max = 3L)

sol <- ROI_solve(x, solver = "msbinlp", method = "glpk",
                 nsol_max = 3L, nsol_add = TRUE)
sol
solution(sol)
sapply(solution(sol), objective(x))

sol <- ROI_solve(x, nsol_max = 2L, nsol_add =  TRUE)
solution(sol)
sapply(solution(sol), objective(x))

sol <- ROI_solve(x, nsol_max = 1L)
solution(sol)
objective(x)(solution(sol))

## solutions from the paper
sol_1 <- c(1, 0, 1, 0, 1)
sol_2 <- c(0, 1, 1, 0, 1)

objective(x)(sol_1)
objective(x)(sol_1)


types(x)

ROI:::OP_signature(x)


OP_signature <- ROI:::OP_signature
attach(getNamespace("ROI"))



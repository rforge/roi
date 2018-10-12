q("no")
Rdevel

library(ROI.plugin.qpoases)
attach(getNamespace("ROI.plugin.qpoases"))

model <- qproblem(2, 1, 0)

H <- c(1.0, 0.0, 0.0, 0.5)
A <- c(1.0, 1.0)
g <- c(1.5, 1.0)
lb  <- c(0.5, -2.0)
ub  <- c(5.0,  2.0)
lbA <- c(-1.0)
ubA <- c( 2.0)

H
g
A[1:2]
lb
ub
lbA
ubA

init_qproblem(model, H, g, A, lb, ub, lbA, ubA, 10, 0)
print_options(model)
get_primal_solution(model)

status_codes[]

print_options(model)

get_number_of_variables(model)
get_number_of_constraints(model)

is_solved(model)

get_primal_solution(model)
get_dual_solution(model)
get_objval(model)

H2 <- matrix(H, 2)
as.vector(H2)
as.vector(t(H2))


library(ROI)

H <- c(1.0, 0.0, 0.0, 0.5)
A <- c(1.0, 1.0)
g <- c(1.5, 1.0)
lb  <- c(0.5, -2.0)
ub  <- c(5.0,  2.0)
lbA <- c(-1.0)
ubA <- c( 2.0)

m <- OP()
objective(m) <- Q_objective(Q = matrix(H, 2, byrow = TRUE), L = g)
bounds(m) <- V_bound(li = 1:2, ui = 1:2, lb = lb, ub = ub)
constraints(m) <- L_constraint(rbind(A, A), c(">=", "<="), c(-1, 2))
x <- m

nrow(constraints(m))
ncol(constraints(m))
str(m)

control <- list(hessian_type = 2L)

x <- ROI_solve(m, solver = "qpoases", hessian_type = 2L)
solution(x)
solution(x, "msg")
solution(x, "status")

sum(solution(x))

(x <- ROI_solve(m, solver = "gurobi"))
solution(x)
solution(x, "msg")
solution(x, "status")

(x <- ROI_solve(m, solver = "cplex"))
solution(x)
solution(x, "msg")
solution(x, "status")

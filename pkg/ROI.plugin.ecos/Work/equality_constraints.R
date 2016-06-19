library(ROI)

lo <- L_objective(1:3)
lc <- L_constraint(c(1, 1, 1), dir="==", rhs=3)
op <- OP(lo, lc)
ROI_solve(op, solver="glpk")$solution
ROI_solve(op, solver="ecos")$solution
ROI_solve(op, solver="ecos")$message$s
ROI_solve(op, solver="scs")$solution

lo <- L_objective(1:3)
lc <- L_constraint(c(1, 1, 1), dir="==", rhs=3)
op <- OP(lo, lc)
ROI_solve(op, solver="glpk")$solution
ROI_solve(op, solver="ecos")$solution



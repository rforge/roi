q("no")
Rdevel

library(ROI)
library(ROI.tests)

test_solver("ecos")
test_solver("scs")

test_solver("glpk")

test_solver("quadprog")
test_solver("ipop")
test_solver("symphony")
test_solver("nloptr")

test_solver("lpsolve")

test_solver("gurobi")
test_solver("mosek")

traceback()
ls()

ROI_registered_solvers()



library(ECOSolveR)
ecos.control()



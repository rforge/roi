q("no")
Rdevel

library(ROI)
library(ROI.tests)

as.list(ROI.tests:::TESTS)


test_solver("scs")
test_solver("glpk")

test_solver("quadprog")

test_solver("nloptr")





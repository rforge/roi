
q("no")
Rdevel

library(ROI)
library(ROI.models.netlib)

prob <- get_test_problem("agg.rda")

x <- prob$op
prob$optimal_value

glpk_sol <- ROI_solve(x, "glpk")
str(glpk_sol)
glpk_sol$objval
glpk_sol$solution

scs_sol <- ROI_solve(x, "scs", control=list(max_iters=10000L, scale=1))
str(scs_sol)
scs_sol$objval
scs_sol$solution
scs_sol$status
class(scs_sol)

ecos_sol <- ROI_solve(x, "ecos")
str(ecos_sol)
ecos_sol$objval
ecos_sol$solution
ecos_sol$status


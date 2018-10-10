q("no")
Rdevel

library(ROI)
library(ROI.tests)

solver <- "alabama"

## nlminb
test_solver("nlminb") ## OK

## alabama
test_solver("alabama") ## OK

## cbc
test_solver("cbc") ## 

ROI_registered_solver_control("cbc")

## clp
test_solver("clp") ## ERROR: In LP-02.

## cplex
test_solver("cplex") ## OK

## deoptim
test_solver("deoptim") ## OK

## ecos
test_solver("ecos") ## OK

## glpk
test_solver("glpk") ## OK

## gurobi
test_solver("gurobi") ## OK

## ipop
test_solver("ipop") ## ERROR: Everywhere!

## lpsolve
test_solver("lpsolve")

## mosek
test_solver("mosek") ## ERROR: In QP!

## msbinlp
test_solver("msbinlp") ## NOTE: No test is done!

## neos
## test_solver("neos")

## nloptr
test_solver("nloptr") ## ERROR: x0 is missing

## optimx
test_solver("optimx") ## ERROR: start value is missing

## qpoases
test_solver("qpoases") ## ERRÓR: Q matrix is missing!

## quadprog
test_solver("quadprog") ## OK

## scs
test_solver("scs") ## OK

## symphony
test_solver("symphony")





traceback()
ls()

as.list(m)

ROI_registered_solvers()



library(ECOSolveR)
ecos.control()


attach(getNamespace("ROI"))
OP_signature(x)
signatures <- OP_signature(x)

ROI_plugin_make_signature


attach(getNamespace("ROI.tests"))
solver <- "alabama"
solver <- "glpk"
test_lp_01(solver)

solver_control("alabama", 1)
solver_control("scs", 1)

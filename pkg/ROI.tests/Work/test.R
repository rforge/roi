q("no")
R

library(ROI)
library(ROI.tests)

solver <- "alabama"

## nlminb
test_solver("nlminb") ## OK

## alabama
test_solver("alabama") ## OK

## cbc
test_solver("cbc") ## ERRORs

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
test_solver("ipop") ## ERROR: No real error but the solver gets all the solutions wrong!

## lpsolve
test_solver("lpsolve") ## OK

## mosek
test_solver("mosek") ## OK

## msbinlp
test_solver("msbinlp") ## NOTE: No test is done!

## neos
## test_solver("neos")

## nloptr
test_solver("nloptr") ## ERROR: x0 is missing

## optimx
test_solver("optimx") ## OK

## qpoases
test_solver("qpoases") ## ERRÓR: Q matrix is missing!

## quadprog
test_solver("quadprog") ## OK

## scs
test_solver("scs") ## OK

## symphony
test_solver("symphony") ## OK

## deoptim
test_solver("deoptim")  ## ERROR - Not an error from the plugin but just the solver gives the wrong solution.
test_solver("deoptimr") ## ERROR - Not an error from the plugin but just the solver gives the wrong solution.


## nloptr
test_solver("nloptr.bobyqa")
test_solver("nloptr.crs2lm")
test_solver("nloptr.direct")
test_solver("nloptr.directL")
## test_solver("nloptr.direct_parallel")
test_solver("nloptr.lbfgs")
test_solver("nloptr.neldermead")
test_solver("nloptr.newuoa")
test_solver("nloptr.sbplx")
test_solver("nloptr.stogo")
test_solver("nloptr.tnewton")
test_solver("nloptr.varmetric")
test_solver("nloptr.cobyla")
test_solver("nloptr.mma")
test_solver("nloptr.auglag")
test_solver("nloptr.isres")
test_solver("nloptr.slsqp")



test_solver("cplex")
test_solver("gurobi")
test_solver("mosek")


solver <- "deoptim"

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

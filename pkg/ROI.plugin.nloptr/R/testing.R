if ( FALSE ) {

setwd("/home/florian/work/Optimization/ROI/src/roi/pkg/ROI.plugin.nloptr/R")
source("constraints.R")

## test constraints
f1 <- function(x) x
f2 <- function(x) 2 * x
f3 <- function(x) 3 * x

eq_fc <- F_constraint(F=list(f1, f2, f3), dir=rep.int("==", 3), rhs=1:3)
x <- OP(objective=F_objective(F=function(x) sum(x), n=3L), eq_fc)

control <- list(start=0)

##' -------------------------------------
##' Example 1
##' -------------------------------------
##' min x^2
##' s.t. x >= 5
##' Solution: 5
control <- list(start=0)
x <- OP(objective = F_objective(F=function(x) 1 + x^2, n=1L))


##' -------------------------------------
##' Example 2
##' -------------------------------------
##' min x^2
##' s.t. x >= 5
##' Solution: 5

control <- list(start=0)
x <- OP(objective = F_objective(F=function(x) x^2, n=1L),
        constraints =  F_constraint(F=function(x) -x, dir="<=", rhs=-5) )


.solve_NLP_nloptr_lbfgs(x, list())



}


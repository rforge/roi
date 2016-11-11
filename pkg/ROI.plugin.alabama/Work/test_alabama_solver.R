## Copyright (C) 2016 Florian Schwendinger
## Copyright (C) 2010 Jelmer Ypma. All Rights Reserved.
## This code is published under the L-GPL.
##
## File:   hs071.R
## Author: Jelmer Ypma
## Date:   10 June 2010
##
## Example problem, number 71 from the Hock-Schittkowsky test suite.
##
## \min_{x} x1*x4*(x1 + x2 + x3) + x3
## s.t.
##    x1*x2*x3*x4 >= 25
##    x1^2 + x2^2 + x3^2 + x4^2 = 40
##    1 <= x1,x2,x3,x4 <= 5
## 
## we re-write the inequality as
##   25 - x1*x2*x3*x4 <= 0
##
## and the equality as
##   x1^2 + x2^2 + x3^2 + x4^2 - 40 = 0
##
## x0 = (1,5,5,1)
##
## Optimal solution = (1.00000000, 4.74299963, 3.82114998, 1.37940829)
##
## CHANGELOG:
##   05/05/2014: Changed example to use unit testing framework testthat.
##   08/06/2016: Changed into the ROI format.

library(alabama)

f_objective <- function(x) x[1]*x[4]*(x[1] + x[2] + x[3]) + x[3]
f_gradient <- function(x) c( x[1] * x[4] + x[4] * (x[1] + x[2] + x[3]),
                             x[1] * x[4],
                             x[1] * x[4] + 1.0,
                             x[1] * (x[1] + x[2] + x[3]) )

## Inequality constraints.
g_leq_constraints <- function(x) c(x[1] * x[2] * x[3] * x[4] - 25)


g_leq_jacobian <- function(x) rbind(c( -x[2] * x[3] * x[4],
                                       -x[1] * x[3] * x[4],
                                       -x[1] * x[2] * x[4],
                                       -x[1] * x[2] * x[3] ))

## Equality constraints.
h_eq_constraints <- function(x) x[1]^2 + x[2]^2 + x[3]^2 + x[4]^2 - 40

h_eq_jacobian <- function(x) rbind(c( 2.0 * x[1],
                                      2.0 * x[2],
                                      2.0 * x[3],
                                      2.0 * x[4] ))



start <- c( 1, 5, 5, 1 )

args(auglag)
opt <- auglag(par=start, fn=f_objective, gr=f_gradient, 
              hin=hin, hin.jac=hin.jac, 
              heq=h_eq_constraints, heq.jac=h_eq_jacobian)

opt$par
opt <- auglag(par=start, fn=f_objective, gr=f_gradient, 
              hin=g_leq_constraints, heq=h_eq_constraints, 
              control.optim=list(method="nlminb", lower=rep.int(1, 4), upper=rep.int(5, 4)))
opt$par

g_leq_constraints <- function(x) c(x[1] * x[2] * x[3] * x[4] - 25, x - 1, 5 - x)
opt <- auglag(par=start, fn=f_objective, gr=f_gradient, 
              hin=g_leq_constraints, heq=h_eq_constraints)
opt$par
f_objective(opt$par)
f_objective(c(1.00000000, 4.74299963, 3.82114998, 1.37940829))

g_leq_constraints(opt$par)
h_eq_constraints(opt$par)

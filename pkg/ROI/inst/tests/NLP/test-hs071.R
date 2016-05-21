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


context("HS071")

test_that( "Test HS071.", {
    
    ##
    ## f(x) = x1*x4*(x1 + x2 + x3) + x3
    ##
    f_objective <- function(x) x[1]*x[4]*(x[1] + x[2] + x[3]) + x[3]
    f_gradient <- function(x) c( x[1] * x[4] + x[4] * (x[1] + x[2] + x[3]),
                                 x[1] * x[4],
                                 x[1] * x[4] + 1.0,
                                 x[1] * (x[1] + x[2] + x[3]) )

    ## Inequality constraints.
    g_leq_constraints <- function(x) c( 25 - x[1] * x[2] * x[3] * x[4] )

    g_leq_jacobian <- function(x) c( -x[2] * x[3] * x[4],
                                     -x[1] * x[3] * x[4],
                                     -x[1] * x[2] * x[4],
                                     -x[1] * x[2] * x[3] )
    
    ## Equality constraints.
    h_eq_constraints <- function(x) x[1]^2 + x[2]^2 + x[3]^2 + x[4]^2 - 40

    h_eq_jacobian <- function(x) c( 2.0 * x[1],
                                    2.0 * x[2],
                                    2.0 * x[3],
                                    2.0 * x[4] )

    ## Optimal solution.
    solution.opt <- c(1.00000000, 4.74299963, 3.82114998, 1.37940829)
    
    ## Set optimization options.
    local_opts <- list( "algorithm" = "NLOPT_LD_MMA",
                        "xtol_rel"  = 1.0e-7 )
    opts <- list( "algorithm"   = "NLOPT_LD_AUGLAG",
                  "xtol_rel"    = 1.0e-7,
                  "maxeval"     = 1000,
                  "local_opts"  = local_opts,
                  "print_level" = 0 )

    control <- c( opts, list( start = c( 1, 5, 5, 1 ) ) )

    ## Solve using NLOPT_LD_MMA with gradient information supplied in separate function
    x <- OP(objective = F_objective(F=f_objective, n=4L, G=f_gradient), 
            constraints = F_constraint(F=c(g_leq_constraints, h_eq_constraints), 
                                       dir=c("<=", "=="), rhs=c(0, 0),
                                       J=c(g_leq_jacobian, h_eq_jacobian)),
            bounds = V_bound(li=1:4, ui=1:4, lb=rep.int(1, 4), ub=rep.int(5, 4)) )

    ## Solve Rosenbrock Banana function.
    res <- ROI_solve( x, solver="nloptr", control)
    
    # Run some checks on the optimal solution.
    expect_that( res$solution, equals( solution.opt, tolerance = 1e-6 ) )
    expect_that( all( res$solution >= bounds(x)$lower$val ), is_true() )
    expect_that( all( res$solution <= bounds(x)$upper$val ), is_true() )
    
    # Check whether constraints are violated (up to specified tolerance).
    expect_that( g_leq_constraints( res$solution ) <= 1e-8, is_true() )
    expect_that( h_eq_constraints( res$solution ) <= 1e-8, is_true() )
} )

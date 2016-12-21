## Copyright (C) 2016 Florian Schwendinger. All Rights Reserved.
## This code is published under the L-GPL.
##
## Copyright (C) 2010 Jelmer Ypma. All Rights Reserved.
## This code is published under the L-GPL.
##
## File:   test-banana.R
##
## Changed for ROI: Florian Schwendinger
## Date: 21.02.2016 
##
## Original Author: Jelmer Ypma
## Date:   10 June 2010
##
## Example showing how to solve the Rosenbrock Banana function.
##
## Changelog:
##   27/10/2013: Changed example to use unit testing framework testthat.
##   21/02/2016: Changed it for ROI.

context("Banana")

test_that("Test Rosenbrock Banana optimization with objective and gradient in separate functions.", {

    library( testthat )
    library( ROI )
        
    control <- list( algorithm   = "NLOPT_LD_LBFGS",
                     xtol_rel    = 1.0e-8,
                     print_level = 0 )
    ## initial values
    control$start <- c( -1.2, 1 )
    
    ## Rosenbrock Banana function and gradient in separate functions
    f_objective <- function(x) 100 * (x[2] - x[1] * x[1])^2 + (1 - x[1])^2
    f_gradient <- function(x) {
        c( -400 *  x[1] * (x[2] - x[1] * x[1]) - 2 * (1 - x[1]),
            200 * (x[2] -  x[1] * x[1]) )
    }
    x <- OP( objective = F_objective(F=f_objective, n=2L, G=f_gradient),
             bounds = V_bound(1:2, 1:2, rep(-Inf, 2), rep(Inf, 2)) )

    ## Solve Rosenbrock Banana function.
    res <- ROI_solve( x, solver="nloptr", control )
    ## str(res)

    ## Check results.
    expect_that( res$objval, equals( 0.0 ) )
    expect_that( res$solution, equals( c( 1.0, 1.0 ) ) )
} )

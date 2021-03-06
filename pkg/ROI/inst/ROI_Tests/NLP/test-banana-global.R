## Copyright (C) 2011 Jelmer Ypma. All Rights Reserved.
## This code is published under the L-GPL.
##
## File:   test-banana-global.R
## Author: Jelmer Ypma
## Date:   8 August 2011
##
## Example showing how to solve the Rosenbrock Banana function
## using a global optimization algorithm.
##
## Changelog:
##   27/10/2013: Changed example to use unit testing framework testthat.

context("Banana Global")

## Rosenbrock Banana objective function
eval_f <- function(x) {
    return( 100 * (x[2] - x[1] * x[1])^2 + (1 - x[1])^2 )
}

eval_grad_f <- function(x) {
    return( c( -400 * x[1] * (x[2] - x[1] * x[1]) - 2 * (1 - x[1]),
               200 * (x[2] - x[1] * x[1])) )
}

## initial values
x0 <- c( -1.2, 1 )

## lower and upper bounds
lb <- c( -3, -3 )
ub <- c(  3,  3 )

test_that("Test Rosenbrock Banana optimization with global optimizer NLOPT_GD_MLSL.", {
    # Define optimizer options.
    local_opts <- list( algorithm = "NLOPT_LD_LBFGS",
                        xtol_rel  = 1e-4 )
    
    opts <- list( algorithm  = "NLOPT_GD_MLSL",
                  maxeval    = 10000,
                  population = 4,
                  local_opts = local_opts )
    control <- c(opts, start=list(x0))

    x <- OP( objective = F_objective(eval_f, n=1L, G=eval_grad_f), 
             bounds = V_bound(li=1:2, ui=1:2, lb=lb, ub=ub) )
    
    # Solve Rosenbrock Banana function.
    res <- ROI_solve(x, solver="nloptr", control)
    terms(objective(x))
    
    # Check results.
    expect_that( res$objval, equals( 0.0 ) )
    expect_that( res$solution, equals( c( 1.0, 1.0 ) ) )
} )

test_that("Test Rosenbrock Banana optimization with global optimizer NLOPT_GN_ISRES.", {
    # Define optimizer options.
    # For unit testing we want to set the random seed for replicability.
    opts <- list( algorithm  = "NLOPT_GN_ISRES",
                  maxeval    = 10000,
                  population = 100,
                  ranseed    = 2718 )
    control <- c(opts, start=list(x0))

    x <- OP( objective = F_objective(eval_f, n=1L), 
             bounds = V_bound(li=1:2, ui=1:2, lb=lb, ub=ub) )

    # Solve Rosenbrock Banana function.
    res <- ROI_solve(x, solver="nloptr", control)

    # Check results.
    expect_that( res$objval, equals( 0.0, tolerance=1e-4 ) )
    expect_that( res$solution, equals( c( 1.0, 1.0 ), tolerance=1e-2 ) )
} )

test_that("Test Rosenbrock Banana optimization with global optimizer NLOPT_GN_CRS2_LM with random seed defined.", {
    # Define optimizer options.
    # For unit testing we want to set the random seed for replicability.
    opts <- list( algorithm  = "NLOPT_GN_CRS2_LM",
                  maxeval    = 10000,
                  population = 100,
                  ranseed    = 2718 )
    control <- c(opts, start=list(x0))

    x <- OP( objective = F_objective(eval_f, n=1L), 
             bounds = V_bound(li=1:2, ui=1:2, lb=lb, ub=ub) )

    # Solve Rosenbrock Banana function.
    res1 <- ROI_solve(x, solver="nloptr", control)    

    # Define optimizer options.
    # this optimization uses a different seed for the
    # random number generator and gives a different result
    opts <- list( algorithm  = "NLOPT_GN_CRS2_LM",
                  maxeval    = 10000,
                  population = 100,
                  ranseed    = 3141 )
    control <- c(opts, start=list(x0))
    
    # Solve Rosenbrock Banana function.
    res2 <- ROI_solve(x, solver="nloptr", control)    

    # Define optimizer options.
    # this optimization uses the same seed for the random
    # number generator and gives the same results as res2
    opts <- list( algorithm  = "NLOPT_GN_CRS2_LM",
                  maxeval    = 10000,
                  population = 100,
                  ranseed    = 3141 )
    control <- c(opts, start=list(x0))
    
    # Solve Rosenbrock Banana function.
    res3 <- ROI_solve(x, solver="nloptr", control)    
    
    # Check results.
    expect_that( res1$objval, equals( 0.0, tolerance=1e-4 ) )
    expect_that( res1$solution, equals( c( 1.0, 1.0 ), tolerance=1e-2 ) )
    
    expect_that( res2$objval, equals( 0.0, tolerance=1e-4 ) )
    expect_that( res2$solution, equals( c( 1.0, 1.0 ), tolerance=1e-2 ) )
    
    expect_that( res3$objval, equals( 0.0, tolerance=1e-4 ) )
    expect_that( res3$solution, equals( c( 1.0, 1.0 ), tolerance=1e-2 ) )
    
    # Expect that the results are different for res1 and res2.
    expect_that( res1$objval == res2$objval, is_false() )
    expect_that( all( res1$solution  == res2$solution ), is_false() )
    
    # Expect that the results are identical for res2 and res3.
    expect_that( res2$objval, is_identical_to( res3$objval ) )
    expect_that( res2$solution,  is_identical_to( res3$solution ) )
} )

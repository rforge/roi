## Copyright (C) 2010 Jelmer Ypma. All Rights Reserved.
## This code is published under the L-GPL.
##
## File:   hs023.R
## Author: Jelmer Ypma
## Date:   16 August 2010
##
## Example problem, number 23 from the Hock-Schittkowsky test suite..
##
## \min_{x} x1^2 + x2^2
## s.t.
##   x1 + x2 >= 1
##   x1^2 + x2^2 >= 1
##   9*x1^2 + x2^2 >= 9
##   x1^2 - x2 >= 0
##   x2^2 - x1 >= 0
##
## with bounds on the variables
##   -50 <= x1, x2 <= 50
##
## we re-write the inequalities as
##   1 - x1 - x2 <= 0
##   1 - x1^2 - x2^2 <= 0
##   9 - 9*x1^2 - x2^2 <= 0
##   x2 - x1^2 <= 0
##   x1 - x2^2 <= 0 
##
## the initial value is
## x0 = (3, 1)
##
## Optimal solution = (1, 1)
##
## CHANGELOG:
##   05/05/2014: Changed example to use unit testing framework testthat.

context("HS023")

test_that( "Test HS023.", {
    ##
    ## f(x) = x1^2 + x2^2
    ##
    eval_f <- function( x ) { 
        return( list( "objective" = x[1]^2 + x[2]^2,
                      "gradient" = c( 2*x[1],
                                      2*x[2] ) ) ) 
    }

    f_objective <- function(x) x[1]^2 + x[2]^2
    f_gradient <- function(x) c( 2*x[1], 2*x[2] )
    
    ## Inequality constraints.
    eval_g_ineq <- function( x ) {
        constr <- c( 1 - x[1] - x[2],
                     1 - x[1]^2 - x[2]^2,
                     9 - 9*x[1]^2 - x[2]^2,
                     x[2] - x[1]^2,
                     x[1] - x[2]^2 )
                     
        grad   <- rbind( c( -1, -1 ),
                         c( -2*x[1], -2*x[2] ),
                         c( -18*x[1], -2*x[2] ),
                         c( -2*x[1], 1 ),
                         c( 1, -2*x[2] ) )
                         
        return( list( "constraints"=constr, "jacobian"=grad ) )
    }

    g_constraint <- function(x) {
        c( 1 -   x[1]   - x[2]  ,
           1 -   x[1]^2 - x[2]^2,
           9 - 9*x[1]^2 - x[2]^2,
             -   x[1]^2 + x[2]  ,
                 x[1]   - x[2]^2 )
    }

    g_jacobian <- function(x) {
        rbind( c( -      1,      -1 ),
               c( - 2*x[1], -2*x[2] ),
               c( -18*x[1], -2*x[2] ),
               c( - 2*x[1],       1 ),
               c(        1, -2*x[2] ) )
    }
       
    ## Optimal solution.
    solution.opt <- c( 1, 1 )
    
    # Solve with MMA.
    opts <- list( algorithm            = "NLOPT_LD_MMA",
                  xtol_rel             = 1.0e-7, 
                  tol_constraints_ineq = rep( 1.0e-6, 5 ),
                  print_level          = 0 )

    control <- c(opts, list( start = c( 3, 1 ) ))

    ## Solve using NLOPT_LD_MMA with gradient information supplied in separate function
    x <- OP(objective = F_objective(F=f_objective, n=2L, G=f_gradient), 
            constraints = F_constraint(F=g_constraint, dir="<=", rhs=0, J=g_jacobian),
            bounds = V_bound(li=1:2, ui=1:2, lb=c(-50,-50), ub=c(50,50)) )

    ## Solve Rosenbrock Banana function.
    res <- ROI_solve( x, solver="nloptr", control)
    
    # Run some checks on the optimal solution.
    expect_that( res$solution, equals( solution.opt, tolerance = 1e-7 ) )
    expect_that( all( res$solution >= bounds(x)$lower$val ), is_true() )
    expect_that( all( res$solution <= bounds(x)$upper$val ), is_true() )
    
    # Check whether constraints are violated (up to specified tolerance).
    expect_that( all( g_constraint( res$solution ) <= control$tol_constraints_ineq ), is_true() )
} )

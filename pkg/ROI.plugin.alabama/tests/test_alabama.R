stopifnot(require(alabama))

library(ROI)
library(ROI.plugin.alabama)

check <- function(domain, condition, level=1, message="", call=sys.call(-1L)) {
    if ( isTRUE(condition) ) return(invisible(NULL))
    msg <- sprintf("in %s", domain)
    if ( all(nchar(message) > 0) ) msg <- sprintf("%s\n\t%s", msg, message)
    stop(msg)
    return(invisible(NULL))
}

## Copyright (C) 2016 Florian Schwendinger
## Copyright (C) 2010-14 Jelmer Ypma. All Rights Reserved.
## This code is published under the L-GPL.
##
## File:   test-example.R
## Author: Jelmer Ypma
## Date:   10 June 2010
##
## Example showing how to solve the problem from the NLopt tutorial.
##
## min sqrt( x2 )
## s.t. x2 >= 0
##      x2 >= ( a1*x1 + b1 )^3
##      x2 >= ( a2*x1 + b2 )^3
## where
## a1 = 2, b1 = 0, a2 = -1, b2 = 1
##
## re-formulate constraints to be of form g(x) <= 0
##      ( a1*x1 + b1 )^3 - x2 <= 0
##      ( a2*x1 + b2 )^3 - x2 <= 0
## 
## Optimal solution: ( 1/3, 8/27 )
##
## CHANGELOG:
##   03/05/2014: Changed example to use unit testing framework testthat.
##   08/06/2016: Changed into the ROI format.
test_nlp_02 <- function() {
    # Define parameters.
    a <- c( 2, -1 )
    b <- c( 0,  1 )

    ## objective function
    eval_f0 <- function( x ) { 
        return( sqrt(x[2]) )
    }

    ## constraint function
    eval_g0 <- function( x ) {
        return( (a*x[1] + b)^3 - x[2] )
    }

    ## gradient of objective function
    eval_grad_f0 <- function( x ){ 
        return( c( 0, .5/sqrt(x[2]) ) )
    }

    ## jacobian of constraint
    eval_jac_g0 <- function( x ) {
        return( rbind( c( 3*a[1]*(a[1]*x[1] + b[1])^2, -1.0 ), 
                       c( 3*a[2]*(a[2]*x[1] + b[2])^2, -1.0 ) ) )
    }

    # Define optimal solution.
    solution.opt <- c( 1/3, 8/27 )

    ## Solve using NLOPT_LD_MMA with gradient information supplied in separate function
    x <- OP(objective = F_objective(F=eval_f0, n=2L, G=eval_grad_f0), 
            constraints = F_constraint(F=eval_g0, dir=c("<=", "<="), rhs=c(0, 0), J=eval_jac_g0),
            bounds = V_bound(li=1, lb=-Inf))

    ## Solve Rosenbrock Banana function.
    res0 <- ROI_solve( x, solver="alabama", start = c( 1.234, 5.678 ) )
    stopifnot(is.numeric(solution(res0)))

    check("NLP-02@01", equal(solution(res0), solution.opt))
}

## Copyright (C) 2016 Florian Schwendinger
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
##   08/06/2016: Changed into the ROI format.
test_nlp_03 <- function() {
    ##
    ## f(x) = x1^2 + x2^2
    ##
    f_objective <- function(x) x[1]^2 + x[2]^2
    f_gradient <- function(x) c( 2*x[1], 2*x[2] )

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
    
    control <- list( start = c( 3, 1 ) )

    ## Solve using NLOPT_LD_MMA with gradient information supplied in separate function
    x <- OP(objective = F_objective(F=f_objective, n=2L, G=f_gradient), 
            constraints = F_constraint(F=g_constraint, 
                                       dir=rep("<=", 5), rhs=rep(0, 5), 
                                       J=g_jacobian),
            bounds = V_bound(li=1:2, ui=1:2, lb=c(-50,-50), ub=c(50,50)) )
    
    ## Solve Rosenbrock Banana function.
    res <- ROI_solve( x, solver="alabama", control)
    stopifnot(is.numeric(res$solution))
    
    # Run some checks on the optimal solution.
    check("NLP-03@01", equal(res$solution, solution.opt, tol = 1e-1 ))
    check("NLP-03@02", all( res$solution >= bounds(x)$lower$val ))
    check("NLP-03@03", all( res$solution <= bounds(x)$upper$val ))
    
    # Check whether constraints are violated (up to specified tolerance).
    check("NLP-03@04", all( g_constraint( res$solution ) <= control$tol_constraints_ineq ))
}

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
test_nlp_04 <- function() {   
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

    ## Optimal solution.
    solution.opt <- c(1.00000000, 4.74299963, 3.82114998, 1.37940829)
    
    control <- list( start = c( 1, 5, 5, 1 ) )

    ## Solve using NLOPT_LD_MMA with gradient information supplied in separate function
    x <- OP(objective = F_objective(F=f_objective, n=4L, G=f_gradient), 
            constraints = F_constraint(F=c(g_leq_constraints, h_eq_constraints), 
                                       dir=c("<=", "=="), rhs=c(0, 0),
                                       J=c(g_leq_jacobian, h_eq_jacobian)),
            bounds = V_bound(li=1:4, ui=1:4, lb=rep.int(1, 4), ub=rep.int(5, 4)) )

    ## Solve Rosenbrock Banana function.
    res <- ROI_solve( x, solver="alabama", control )
    
    stopifnot(is.numeric(solution(res)))
    
    # Run some checks on the optimal solution.
    check("NLP-04@01", equal(solution(res), solution.opt, tol = 1e-4 ))
    ## check result >= lower bound
    eps <- 1e-7 ## alabama::auglag @default
    check("NLP-04@02", all( solution(res) >= (bounds(x)$lower$val - eps) ))
    check("NLP-04@03", all( solution(res) <= (bounds(x)$upper$val + eps) ))
    
    # Check whether constraints are violated (up to specified tolerance).
    check("NLP-04@04", isTRUE(g_leq_constraints( solution(res) ) <= eps))
    check("NLP-04@05", isTRUE(h_eq_constraints( solution(res) ) <= eps))
}

## Copyright (C) 2016 Florian Schwendinger
## Copyright (C) 2010 Jelmer Ypma. All Rights Reserved.
## This code is published under the L-GPL.
##
## File:   simple.R
## Author: Jelmer Ypma
## Date:   20 June 2010
##
## Example showing how to solve a simple constrained problem.
##
## min x^2
## s.t. x >= 5
##
## re-formulate constraint to be of form g(x) <= 0
##      5 - x <= 0
## we could use a bound constraint as well here
##
## CHANGELOG:
##   05/05/2014: Changed example to use unit testing framework testthat.
##   08/06/2016: Changed into the ROI format.
test_nlp_06 <- function() {   
    ## -----------------------------------------------------
    ## Test simple constrained optimization problem with gradient information.
    ## -----------------------------------------------------
    ## Objective function.
    eval_f <- function(x) { 
        return( x^2 )
    }
    
    ## Gradient of objective function.
    eval_grad_f <- function(x) { 
        return( 2*x )
    }
    
    ## Inequality constraint function.
    eval_g_ineq <- function( x ) {
        return( 5 - x )
    }
    
    ## Jacobian of constraint.
    eval_jac_g_ineq <- function( x ) {
        return( rbind(-1) )
    }
    
    ## Optimal solution.
    solution.opt <- 5

    control <- list( xtol_rel = 1e-4, algorithm = "NLOPT_LD_MMA", start = 1 )

    x <- OP( objective = F_objective(F=eval_f, n=1L, G=eval_grad_f),
             constraints = F_constraint(F=eval_g_ineq, dir="<=", rhs=0, J=eval_jac_g_ineq),
             bounds = V_bound(1, 1, -Inf, Inf) )


    ## Solve using NLOPT_LD_MMA with gradient information supplied in separate function
    res <- ROI_solve( x, solver="alabama", control)
    stopifnot(is.numeric(res$solution))

    ## Run some checks on the optimal solution.
    check("NLP-06@01", equal(res$solution, solution.opt))
    
    ## Check whether constraints are violated (up to specified tolerance).
    check("NLP-06@02", isTRUE(eval_g_ineq( res$solution ) <= 1e-7))
}

## Copyright (C) 2016 Florian Schwendinger
## Copyright (C) 2010 Jelmer Ypma. All Rights Reserved.
## This code is published under the L-GPL.
##
## File:   systemofeq.R
## Author: Jelmer Ypma
## Date:   20 June 2010
##
## Example showing how to solve a system of equations.
##
## min 1
## s.t. x^2 + x - 1 = 0
##
## Optimal solution for x: -1.61803398875
##
## CHANGELOG:
##   16/06/2011: added NLOPT_LD_SLSQP
##   05/05/2014: Changed example to use unit testing framework testthat.
##   08/06/2016: Changed into the ROI format.
test_nlp_07 <- function() {
    ## -----------------------------------------------------
    ## Solve system of equations using NLOPT_LD_MMA with local optimizer NLOPT_LD_MMA.
    ## -----------------------------------------------------
    # Objective function.
    eval_f0 <- function( x ) { 
        return( 1 )
    }
    
    # Gradient of objective function.
    eval_grad_f0 <- function( x ) { 
        return( 0 )
    }
    
    # Equality constraint function.
    eval_g0_eq <- function( x ) {
        return( params[1]*x^2 + params[2]*x + params[3] )
    }
    
    # Jacobian of constraint.
    eval_jac_g0_eq <- function( x ) {
        return( rbind(2*params[1]*x + params[2]) )
    }
    
    # Define vector with addiitonal data.
    params <- c(1, 1, -1)
    
    # Define optimal solution.
    solution.opt <- -1.61803398875

    x <- OP( objective = F_objective(F=eval_f0, n=1L, G=eval_grad_f0),
             constraints = F_constraint(F=eval_g0_eq, dir="==", rhs=0, J=eval_jac_g0_eq),
             bounds = V_bound(1, 1, -Inf, Inf) )

    res <- ROI_solve( x, solver="alabama", start=-1)

    stopifnot(is.numeric(solution(res)))
        
    # Check whether constraints are violated (up to specified tolerance).
    check("NLP-07@02", equal(abs(eval_g0_eq(res$solution)), 0))

}

## SOURCE: Rglpk manual
## https://cran.r-project.org/web/packages/Rglpk/Rglpk.pdf
## 
## LP - Example - 1
## max:  2 x_1 + 4 x_2 + 3 x_3
## s.t.
## 3 x_1  +  4 x_2  +  2 x_3  <= 60
## 2 x_1  +    x_2  +  2 x_3  <= 40
##   x_1  +  3 x_2  +  2 x_3  <= 80 
## x_1, x_2, x_3 >= 0
test_nlp_08 <- function() {
    ## -----------------------------------------------------
    ## Test transformation from LP to NLP
    ## -----------------------------------------------------
    mat <- matrix(c(3, 4, 2, 2, 1, 2, 1, 3, 2), nrow=3, byrow=TRUE)
    lo <- L_objective(c(2, 4, 3))
    lc <- L_constraint(L = mat, dir = c("<=", "<=", "<="), rhs = c(60, 40, 80))
    lp <- OP(objective = lo, constraints = lc, maximum = TRUE)
    opt.solution <- c(0, 6.66666666666667, 16.6666666666667)

    nlp_opt <- ROI_solve(lp, solver="alabama", start=c(1, 1, 1))
    
    check("NLP-08@01", equal(solution(nlp_opt), opt.solution))
}

if ( !any("alabama" %in% names(ROI_registered_solvers())) ) {
    ## This should never happen.
    cat("ROI.plugin.nloptr cloud not be found among the registered solvers.\n")
} else {
    print("Start Testing!")
    cat("Test 02: ")
    local({test_nlp_02()})
    cat("OK\n"); cat("Test 03: ")
    local({test_nlp_03()})
    cat("OK\n"); cat("Test 04: ")
    local({test_nlp_04()})    
    cat("OK\n"); cat("Test 06: ")
    local({test_nlp_06()})
    cat("OK\n"); cat("Test 07: ")
    local({test_nlp_07()})
    cat("OK\n"); cat("Test 08: \n")
    local({test_nlp_08()})
}

library(ROI)
library(ROI.plugin.scs)


check <- function(domain, condition, level=1, message="", call=sys.call(-1L)) {
    if ( isTRUE(condition) ) return(invisible(NULL))
    msg <- sprintf("in %s", domain)
    if ( all(nchar(message) > 0) ) msg <- sprintf("%s\n\t%s", msg, message)
    stop(msg)
    return(invisible(NULL))
}


## SOCP - Example - 1
## min:  1 x1 + 1 x2 + 1 x3
## s.t.     x1 == sqrt(2)
##          x1 >= ||(x2, x3)||
##
## c(sqrt(2), -1, -1)
test_cp_01 <- function(solver) {
    obj <- c(1, 1, 1)
    A <- rbind(c(1, 0, 0))
    b <- c(sqrt(2))
    G <- diag(x=-1, 3)
    h <- rep(0, 3)

    bound <- V_bound(li = 1:3, lb = rep(-Inf, 3))

    lc <- C_constraint(L = rbind(A, G), 
                       cones = c(K_zero(1), K_soc(3)), 
                       rhs = c(b, h))
    x <- OP(objective = obj, constraints = lc, types = rep("C", 3),
            bounds =  bound, maximum = FALSE)

    opt <- ROI_solve(x, solver = solver)
    
    check("CP-01@01", equal(sum(abs(opt$solution - c(sqrt(2), -1, -1))), 0))
    check("CP-01@02", equal(opt$objval, (sqrt(2) - 2)))
}

## SOCP - Example - 2
## min:  0 x1 - 2 x2 - 2 x3 + 0 x4 - 2 x5 - 2 x6
## s.t.     x1 == sqrt(2)
##          x4 == sqrt(2)
##          x1 >= ||(x2, x3)||
##          x4 >= ||(x5, x6)||
##
## c(sqrt(2), 1, 1, sqrt(2), 1, 1)
test_cp_02 <- function(solver) {
    obj <- c(0, -2, -2, 0, -2, -2)
    A <- rbind(c(1, 0, 0, 0, 0, 0),
               c(0, 0, 0, 1, 0, 0))
    b <- c(sqrt(2), sqrt(2))
    G <- diag(x=-1, 6)
    h <- rep(0, 6)

    lc <- C_constraint(L = rbind(A, G), 
                       cones = c(K_zero(2), K_soc(c(3, 3))), 
                       rhs = c(b, h))
    x <- OP(objective = obj, constraints = lc)

    opt <- ROI_solve(x, solver=solver)
    check("CP-02@01", equal(sum(abs(opt$solution - c(sqrt(2), 1, 1, sqrt(2), 1, 1))), 0))
}

## EXPP - Example - 1
## min:  x + y + z
## s.t.
## y e^(x/y) <= z
## y > 0
## x := 1
## y := 2
## c(1, 2, 2*exp(1/2))
test_cp_03 <- function(solver) {
    obj <- c(1, 1, 1)
    A <- rbind(c(1, 0, 0),
               c(0, 1, 0))
    b <- c(1, 2)
    G <- -diag(3)
    h <- rep(0, 3)

    lc <- C_constraint(L = rbind(A, G), 
                       cones = c(K_zero(2), K_expp(1)), 
                       rhs = c(b, h))
    x <- OP(objective = obj, constraints = lc)

    opt <- ROI_solve(x, solver = solver)
    check("CP-03@01", equal(opt$solution , c(1, 2, 2*exp(1/2))))
}

## EXPP - Example - 2
## max:  x + y + z
## s.t.
## y e^(x/y) <= z
## y > 0
## y == 2
## z == 2 * exp(1/2)
## c(1, 2, 2*exp(1/2))
test_cp_04 <- function(solver) {
    obj <- c(1, 1, 1)
    A <- rbind(c(0, 1, 0),
               c(0, 0, 1))
    b <- c(2, 2*exp(1/2))
    G <- diag(x=-1, 3)
    h <- rep(0, 3)

    lc <- C_constraint(L = rbind(A, G), 
                       cones = c(K_zero(2), K_expp(1)), 
                       rhs = c(b, h))
    x <- OP(objective = obj, constraints = lc, maximum = TRUE)

    opt <- ROI_solve(x, solver=solver)
    check("CP-04@01", equal(opt$solution , c(1, 2, 2*exp(1/2))))
}

## EXPP - Example - 3
## max:  x + y + z
## s.t.
## y e^(x/y) <= z
## y > 0
## y == 1
## z == exp(1)
## c(1, 1, exp(1))
test_cp_05 <- function(solver) {
    obj <- c(1, 1, 1)
    A <- rbind(c(0, 1, 0),
               c(0, 0, 1))
    b <- c(1, exp(1))
    G <- diag(x=-1, 3)
    h <- rep(0, 3)
    ## cones <- list("free"=c(1, 2), "expp"=list(3:5))
    ## bound <- as.C_bound(cones)

    lc <- C_constraint(L = rbind(A, G), 
                       cones = c(K_zero(2), K_expp(1)), 
                       rhs = c(b, h))
    x <- OP(objective = obj, constraints = lc, 
            types = rep("C", 3), maximum = TRUE)

    opt <- ROI_solve(x, solver = solver)
    check("CP-05@01", equal(opt$solution , c(1, 1, exp(1))))
}

## EXPD - Example - 1
## min:  u + v + w
## s.t.
##      -u * e^(v/u) <= e * w; u < 0; v >= 0; w >= 0
##      u == -1
##      v ==  1
##
##      c(-1, 1, exp(-2))
test_cp_06 <- function(solver) {
    x <- OP(c(1, 1, 1))
    A <- rbind(c(1,  0, 0),
               c(0,  1, 0))
    b <- c(-1, 1)
    G <- diag(x=-1, 3)
    h <- rep(0, 3)
    constraints(x) <- C_constraint(L = rbind(A, G), 
                                   cones = c(K_zero(2), K_expd(1)), 
                                   rhs = c(b, h))
    bounds(x) <- V_bound(li=1:3, lb=rep(-Inf, 3))

    opt <- ROI_solve(x, solver=solver)

    check("CP-06@01", equal(opt$solution , c(-1, 1, exp(-2))))
}

## POWP - Example - 1
## max:  x + y + z
## s.t.
##      x^a * y ^ (1-a) >= |z|
##      x == 4
##      y == 4
##      a == 1/2
##
## c(4, 4, 4)
test_cp_07 <- function(solver) {
    obj <- c(1, 1, 1)
    A <- rbind(c(1, 0, 0),
               c(0, 1, 0))
    b <- c(4, 4)
    G <- diag(x=-1, 3)
    h <- rep(0, 3)

    cc <- C_constraint(L = rbind(A, G), 
                       cones = c(K_zero(2), K_powp(0.5)), 
                       rhs = c(b, h))
    x <- OP(objective = obj, constraints = cc, 
            types = rep("C", 3), maximum = TRUE)

    opt <- ROI_solve(x, solver=solver)
    check("CP-07@01", equal(opt$solution, c(4, 4, 4)))
    check("CP-07@02", equal(opt$objval, 12 ))
}

## POWD - Example - 1
## min:  u + v + w
## s.t.
##      (u/a)^a * ( v/(1-a) )^(1-a) >= |w|
##      u == 2
##      v == 2
##      a == 1/2
##
## c(2, 2, 4)
test_cp_08 <- function(solver) {
    obj <- c(1, 1, 1)
    A <- rbind(c(1, 0, 0),
               c(0, 1, 0))
    b <- c(2, 2)
    G <- diag(x=-1, 3)
    h <- rep(0, 3)
    ## cones <- list("free"=c(1, 2), "powd"=list(list(i=3:5, a=0.5)))
    ## bound <- as.C_bound(cones)

    lc <- C_constraint(L = rbind(A, G), 
                       cones = c(K_zero(2), K_powd(0.5)), 
                       rhs = c(b, h))
    x <- OP(objective = obj, constraints = lc, 
            types = rep("C", 3), maximum = TRUE)

    opt <- ROI_solve(x, solver=solver)
    check("CP-08@01", equal(opt$solution, c(2, 2, 4)))
}

## The following example is from the cvxopt documentation and released under GPL-3
## (c) 2016-2016 Florian Schwendinger
## (c) 2012-2015 M. Andersen and L. Vandenberghe.
## CVXOPT is free software; you can redistribute it and/or modify it under the terms of the 
## GNU General Public License as published by the Free Software Foundation; 
## either version 3 of the License, or (at your option) any later version.

## SDP - Example - 1
## for the example definition see ROI.plugin.scs inst/doc
## or http://cvxopt.org/userguide/coneprog.html
test_cp_09 <- function(solver) {
    ## this function or something similar should go into ROI
    obj <- c(1, -1, 1)
    A1 <- matrix(c(-7, -11, -11,  3), 2)
    A2 <- matrix(c( 7, -18, -18,  8), 2)
    A3 <- matrix(c(-2,  -8,  -8,  1), 2)
    a  <- matrix(c(33,  -9,  -9, 26), 2)
    B1 <- matrix(c(-21, -11,  0, -11,  10,   8,  0,    8, 5), 3)
    B2 <- matrix(c(  0,  10,  16, 10, -10, -10,  16, -10, 3), 3)
    B3 <- matrix(c( -5,   2, -17,  2,  -6,   8, -17,   8, 6), 3)
    b  <- matrix(c( 14,   9,  40,  9,  91,  10,  40,  10,15), 3)

    ## PSD matrices have to be vectorized
    G1 <- vech(A1, A2, A3)
    h1 <- vech(a)
    G2 <- vech(B1, B2, B3)
    h2 <- vech(b)
    h <- c(h1, h2)
    bounds <- V_bound(li=1:3, lb=rep(-Inf, 3)) 

    x <- OP(objective = obj,
            constraints = C_constraint(L = rbind(G1, G2), 
                                       cones = K_psd(c(3, 6)), 
                                       rhs = h),
            types = rep("C", length(obj)),
            bounds =  bounds,
            maximum = FALSE)

    opt <- ROI_solve(x, solver = solver)  
    
    ## NOTE: The solutions I compare with are from cvxopt where I used the default settings,
    ##       therefore it is possible that scs just provides a solution with a smaler eps
    sol <- c(-0.367666090041563, 1.89832827158511, -0.887550426343585)
    check("CP-09@01", isTRUE(c(obj %*% solution(opt)) <= c(obj %*% sol)))
    
    ## solution from cvxopt
    ## [-3.68e-01 1.90e+00 -8.88e-01]
    ## or c(-0.367666090041563, 1.89832827158511, -0.887550426343585)
    check("CP-09@02", isTRUE(sum(abs(solution(opt) - sol)) < 1e-3))

    ## [ 3.96e-03 -4.34e-03]
    ## [-4.34e-03  4.75e-03]
    ## c(0.00396107103000518, -0.00433836779348354, -0.00433836779348354,  0.00475162592559036) 
    sol_psd_1 <- c( 0.00396107103000518, -0.00433836779348354, 
                   -0.00433836779348354,  0.00475162592559036)

    opt_sol_psd_1 <- as.numeric(as.matrix(solution(opt, "psd")[[1]]))
    check("CP-09@03", isTRUE(sum(abs(opt_sol_psd_1 - sol_psd_1)) < 1e-5))
    
    ## [ 5.58e-02 -2.41e-03  2.42e-02]
    ## [-2.41e-03  1.04e-04 -1.05e-03]
    ## [ 2.42e-02 -1.05e-03  1.05e-02]
    ## c(0.0558011514407859, -0.00240909203896524, 0.0242146296992217,  -0.00240909203896524, 
    ##   0.000104021271556218, -0.00104543254168053,  0.0242146296992217, -0.00104543254168053, 
    ##   0.0105078600239678) 
    sol_psd_2 <- c( 0.0558011514407859, -0.00240909203896524,   0.0242146296992217,  
                   -0.00240909203896524, 0.000104021271556218, -0.00104543254168053,  
                    0.0242146296992217, -0.00104543254168053,   0.0105078600239678)
    opt_sol_psd_2 <- as.numeric(as.matrix(solution(opt, "psd")[[2]]))
    check("CP-09@04", isTRUE(sum(abs(opt_sol_psd_2 - sol_psd_2)) < 1e-5))
}


if ( !any("scs" %in% names(ROI_registered_solvers())) ) {
    ## This should never happen.
    cat("ROI.plugin.scs cloud not be found among the registered solvers.\n")
} else {
    print("Start Testing!")
    local({test_cp_01("scs")})
    local({test_cp_02("scs")})
    local({test_cp_03("scs")})
    local({test_cp_04("scs")})
    local({test_cp_05("scs")})
    local({test_cp_06("scs")})
    local({test_cp_07("scs")})
    local({test_cp_08("scs")})
    local({test_cp_09("scs")})
}

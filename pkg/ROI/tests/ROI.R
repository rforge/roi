library(ROI)
library(slam)

## ---------------------------
## Objective
## ---------------------------
test.L_objective <- function() {
    v <- 1:3
    m <- matrix(v, 1)
    slam <- as.simple_triplet_matrix(m)
    nam <- LETTERS[v]
    L_objective(v)
    L_objective(v, nam)
    L_objective(m)
    L_objective(m, nam)
    L_objective(slam)
    L_objective(slam, nam)
}

test.Q_objective <- function() {
    Lv <- 1:3
    Lm <- matrix(Lv, 1)
    Lslam <- as.simple_triplet_matrix(Lm)
    Qm <- diag(length(Lv))
    Qslam <- as.simple_triplet_matrix(Qm)
    nam <- LETTERS[Lv]
    Q_objective(Qm, Lv)
    Q_objective(Qm, Lm)
    Q_objective(Qm, Lslam)
    Q_objective(Qslam, Lv)
    Q_objective(Qslam, Lm)
    Q_objective(Qslam, Lslam)
    Q_objective(Qm, Lv, nam)
    Q_objective(Qm, Lm, nam)
    Q_objective(Qm, Lslam, nam)
    Q_objective(Qslam, Lv, nam)
    Q_objective(Qslam, Lm, nam)
    Q_objective(Qslam, Lslam, nam)
}

test.F_objective <- function() {
    F <- F_objective(sum, 3)
}

## ---------------------------
## Constraints
## ---------------------------
test.NO_constraint <- function() {
    NO_constraint(3)
}

test.L_constraint <- function() {
    L_constraint(matrix(1:3, 1), "==", 2, names=LETTERS[1:3])
}

test.Q_constraint <- function() {
    
}

test.Q_constraint <- function() {
    NO_constraint(3)
}


## ---------------------------
## Types
## ---------------------------


## ---------------------------
## Bounds
## ---------------------------

## ---------------------------
## R-Methods
## ---------------------------
## rbind

## c
test.combine_L_constraints <- function() {
    dim_1 <- sample(1:10, 2)
    dim_2 <- c(sample(1:10, 1), dim_1[2])

    mat_1 <- matrix(sample(1:100, prod(dim_1)), ncol=dim_1[2])
    dir_1 <- sample(c("==", "<=", ">="), dim_1[1], replace=TRUE)
    rhs_1 <- sample(1:100, dim_1[1])

    mat_2 <- matrix(sample(1:100, prod(dim_2)), ncol=dim_2[2])
    dir_2 <- sample(c("==", "<=", ">="), dim_2[1], replace=TRUE)
    rhs_2 <- sample(1:100, dim_2[1])
    
    lc_1 <- L_constraint(L = mat_1, dir = dir_1, rhs = rhs_1)
    lc_2 <- L_constraint(L = mat_2, dir = dir_2, rhs = rhs_2)

    lc_3 <- rbind(lc_1, lc_2)
    stopifnot( ((nrow(lc_1) + nrow(lc_2)) == nrow(lc_3)) )
}

test.combine_F_constraints <- function() {
    fc1 <- F_constraint(F=function(x) x[1] + x[2]^2, ">=", 0,
                        J=function(x) c(1, 2*x[2]))
    fc2 <- F_constraint(F=function(x) x[1]^2 + x[2], ">=", 0,
                        J=function(x) c(2*x[1], x[2]))
    c(fc1, fc2)    
}

## as

## is

## G

## J

## variable.names

## ---------------------------
## Optimization Problems
## ---------------------------

## LP
## ==
test.LP <- function() {
    mat <- matrix(c(3, 4, 2,
                    2, 1, 2,
                    1, 3, 2), nrow=3, byrow=TRUE)
    x <- OP(objective = c(2, 4, 3),
            constraints = L_constraint(L = mat,
                                       dir = c("<=", "<=", "<="),
                                       rhs = c(60, 40, 80)),
            maximum = TRUE)
    
    if ( length(ROI_applicable_solvers(x)) ) {
        sol <- ROI_solve(x)    
        stopifnot( equal(sol$solution, c(0, 20/3, 50/3), tol=1e-4) )
        stopifnot( equal(sol$objval, 230/3, tol=1e-4) ) 
    }  
}

## QP
## ==
test.QP <- function() {
## QP - Example - 1
##
## from the quadprog package
## (c) S original by Berwin A. Turlach R port by Andreas Weingessel
## GPL-3
##
## min: -(0 5 0) %*% x + 1/2 x^T x
## under the constraints:      A^T x >= b
## with b = (-8,2,0)^T
## and      (-4  2  0)
##      A = (-3  1 -2)
##          ( 0  0  1)
## we can use solve.QP as follows:
##
## library(quadprog)
## D <- diag(1, 3)
## d <- c(0, 5, 0)
## A <- cbind(c(-4, -3, 0), 
##            c( 2,  1, 0), 
##            c( 0, -2, 1))
## b <- c(-8, 2, 0)
## 
## sol <- solve.QP(D, d, A, bvec=b)
## deparse(sol$solution)
## deparse(sol$value)
    A <- cbind(c(-4, -3, 0), 
               c( 2,  1, 0), 
               c( 0, -2, 1))
    x <- OP(Q_objective(diag(3), L =  c(0, -5, 0)),
            L_constraint(L = t(A),
                         dir = rep(">=", 3),
                         rhs = c(-8, 2, 0)))

    if ( length(ROI_applicable_solvers(x)) ) {
        sol <- ROI_solve(x)
        solution <- c(0.476190476190476, 1.04761904761905, 2.0952380952381)
        stopifnot( equal(sol$solution, solution) )
        stopifnot( equal(sol$objval, -2.38095238095238) )
    }
}

## CP
## ==

## SOCP
test.SOCP_1 <- function() {
    obj <- c(1, 1, 1)
    A <- rbind(c(1, 0, 0))
    b <- c(sqrt(2))
    G <- diag(x=-1, 3)
    h <- rep(0, 3)
    cones <- list("free"=c(1), "soc"=list(2:4))
    bound <- as.C_bound(cones)
    lc <- L_constraint(L = rbind(A, G), 
                       dir=rep("==", length(c(b, h))), 
                       rhs = c(b, h))
    x <- OP(objective = obj, constraints = lc,
            types = rep("C", 3), bounds =  bound,
            maximum = FALSE)

    if ( length(ROI_applicable_solvers(x)) ) {
        sol <- ROI_solve(x)
        stopifnot( equal(sum(abs(sol$solution - c(sqrt(2), -1, -1))), 0) )
        stopifnot( equal(sol$objval, (sqrt(2) - 2)) )
    }
}

test.SOCP_2 <- function() {
    obj <- c(0, -2, -2, 0, -2, -2)
    A <- rbind(c(1, 0, 0, 0, 0, 0),
               c(0, 0, 0, 1, 0, 0))
    b <- c(sqrt(2), sqrt(2))
    G <- diag(x=-1, 6)
    h <- rep(0, 6)
    cones <- list("free"=c(1, 2), "soc"=list(3:5, 6:8))
    bound <- as.C_bound(cones)
    lc <- L_constraint(L = rbind(A, G), 
                       dir=rep("==", length(c(b, h))), 
                       rhs = c(b, h))
    x <- OP(objective = obj, constraints = lc,
            types = rep("C", 6), bounds =  bound,
            maximum = FALSE)

    if ( length(ROI_applicable_solvers(x)) ) {
        sol <- ROI_solve(x)
        z <- c(sqrt(2), 1, 1, sqrt(2), 1, 1)
        stopifnot( equal(sum(abs(sol$solution - z)), 0) )
    }
}

## EXPP
test.EXPP_1 <- function() {
    obj <- c(1, 1, 1)
    A <- rbind(c(1, 0, 0),
               c(0, 1, 0))
    b <- c(1, 2)
    G <- diag(x=-1, 3)
    h <- rep(0, 3)
    cones <- list("free"=c(1, 2), "expp"=list(3:5))
    bound <- as.C_bound(cones)
    lc <- L_constraint(L = rbind(A, G), 
                       dir=rep("==", length(c(b, h))), 
                       rhs = c(b, h))
    x <- OP(objective = obj, constraints = lc,
            types = rep("C", 3), bounds =  bound,
            maximum = FALSE)

    if ( length(ROI_applicable_solvers(x)) ) {
        sol <- ROI_solve(x)
        stopifnot( equal(sol$solution , c(1, 2, 2*exp(1/2))) )
    }
}

test.EXPP_2 <- function() {
    obj <- c(1, 1, 1)
    A <- rbind(c(0, 1, 0), c(0, 0, 1))
    b <- c(2, 2*exp(1/2))
    G <- diag(x=-1, 3)
    h <- rep(0, 3)
    cones <- list("free"=c(1, 2), "expp"=list(3:5))
    bound <- as.C_bound(cones)
    lc <- L_constraint(L = rbind(A, G), 
                       dir=rep("==", length(c(b, h))), 
                       rhs = c(b, h))
    x <- OP(objective = obj, constraints = lc,
            types = rep("C", 3), bounds =  bound,
            maximum = TRUE)

    if ( length(ROI_applicable_solvers(x)) ) {
        sol <- ROI_solve(x)
        stopifnot( equal(sol$solution , c(1, 2, 2*exp(1/2))) )
    }
}

test.EXPP_3 <- function() {
    obj <- c(1, 1, 1)
    A <- rbind(c(0, 1, 0), c(0, 0, 1))
    b <- c(1, exp(1))
    G <- diag(x=-1, 3)
    h <- rep(0, 3)
    cones <- list("free"=c(1, 2), "expp"=list(3:5))
    bound <- as.C_bound(cones)
    lc <- L_constraint(L = rbind(A, G), 
                       dir=rep("==", length(c(b, h))), 
                       rhs = c(b, h))
    x <- OP(objective = obj, constraints = lc,
            types = rep("C", 3), bounds =  bound,
            maximum = TRUE)

    if ( length(ROI_applicable_solvers(x)) ) {
        sol <- ROI_solve(x)
        stopifnot( equal(sol$solution , c(1, 1, exp(1))) )
    }
}

## EXPD
test.EXPD_1 <- function() {
    obj <- c(1, 1, 1)
    A <- rbind(c(1,  0, 0),
               c(0,  1, 0))
    b <- c(-1, 1)
    G <- diag(x=-1, 3)
    h <- rep(0, 3)
    cones <- list("free"=c(1, 2), "expd"=list(3:5))
    bound <- as.C_bound(cones)
    lc <- L_constraint(L = rbind(A, G), 
                       dir=rep("==", length(c(b, h))), 
                       rhs = c(b, h))
    x <- OP(objective = obj, constraints = lc,
            types = rep("C", 3), bounds =  bound,
            maximum = FALSE)

    if ( length(ROI_applicable_solvers(x)) ) {
        sol <- ROI_solve(x)
        stopifnot( equal(sol$solution , c(-1, 1, exp(-2))) )
    }
}

## POWP
test.POWP_1 <- function() {
    obj <- c(1, 1, 1)
    A <- rbind(c(1, 0, 0), c(0, 1, 0))
    b <- c(4, 4)
    G <- diag(x=-1, 3)
    h <- rep(0, 3)
    cones <- list("free"=c(1, 2), "powp"=list(list(i=3:5, a=0.5)))
    bound <- as.C_bound(cones)
    lc <- L_constraint(L = rbind(A, G), 
                       dir=rep("==", length(c(b, h))), 
                       rhs = c(b, h))
    x <- OP(objective = obj, constraints = lc,
            types = rep("C", 3), bounds =  bound,
            maximum = TRUE)

    if ( length(ROI_applicable_solvers(x)) ) {
        sol <- ROI_solve(x)
        stopifnot( equal(sol$solution, c(4, 4, 4)) )
        stopifnot( equal(sol$objval, -12) )
    }
}

## POWD
test.POWD_1 <- function() {
    obj <- c(1, 1, 1)
    A <- rbind(c(1, 0, 0), c(0, 1, 0))
    b <- c(2, 2)
    G <- diag(x=-1, 3)
    h <- rep(0, 3)
    cones <- list("free"=c(1, 2), "powd"=list(list(i=3:5, a=0.5)))
    bound <- as.C_bound(cones)
    lc <- L_constraint(L = rbind(A, G), 
                       dir=rep("==", length(c(b, h))), 
                       rhs = c(b, h))
    x <- OP(objective = obj, constraints = lc,
            types = rep("C", 3), bounds =  bound,
            maximum = TRUE)

    if ( length(ROI_applicable_solvers(x)) ) {
        sol <- ROI_solve(x)
        stopifnot( equal(sol$solution, c(2, 2, 4)) )
    }
}

## SDP
test.SDP_1 <- function() {
## The following example is from the cvxopt documentation and released under GPL-3
## (c) 2016-2016 Florian Schwendinger
## (c) 2012-2015 M. Andersen and L. Vandenberghe.
## CVXOPT is free software; you can redistribute it and/or modify it under the terms of the 
## GNU General Public License as published by the Free Software Foundation; 
## either version 3 of the License, or (at your option) any later version.

## SDP - Example - 1
## for the example definition see ROI.plugin.scs inst/doc
## or http://cvxopt.org/userguide/coneprog.html
    obj <- c(-6, -4, -5)
    G <- cbind(c( 16, 7,  24,  -8,   8, -1, 0, -1,  0,  0, 7, -5,  1, -5,  1,  -7,  1,  -7,  -4),
               c(-14, 2,   7, -13, -18,  3, 0,  0, -1,  0, 3, 13, -6, 13, 12, -10, -6, -10, -28),
               c(  5, 0, -15,  12,  -6, 17, 0,  0,  0, -1, 9,  6, -6,  6, -7,  -7, -6,  -7, -11))
    h <- c(-3, 5, 12, -2, -14, -13, 10, 0, 0, 0, 68, -30, -19, -30, 99, 23, -19, 23, 10)

    dims <- c(C_bound(1:6, type="free"), C_bound(7, 8, type="nonneg"), 
              C_bound(9:12, type="soc"), C_bound(13:16, type="soc"), 
              C_bound(17:19, type="psd"))
    lc <- L_constraint(L = G, dir=rep("==", length(h)), rhs = h)
    x <- OP(objective = obj, constraints = lc,
            types = rep("C", length(obj)), bounds =  dims,
            maximum = FALSE)

    if ( length(ROI_applicable_solvers(x)) ) {
        sol <- ROI_solve(x)
        stopifnot( equal(sum(abs(opt$solution - c(sqrt(2), -1, -1))), 0) ) 
        stopifnot( equal(opt$objval, sqrt(2) - 2) )
    }
}

## NLP
## ===
test.NLP_1 <- function() {
    f <- function(x) {
        return( 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2 )
    }

    f.gradient <- function(x) {
        return( c( -400 * x[1] * (x[2] - x[1] * x[1]) - 2 * (1 - x[1]),
                    200 * (x[2] - x[1] * x[1])) )
    }

    x <- OP( objective = F_objective(f, n=2L, G=f.gradient), 
             bounds = V_bound(li=1:2, ui=1:2, lb=c(-3, -3), ub=c(3, 3)) )

    if ( length(ROI_applicable_solvers(x)) ) {
        nlp <- ROI_solve(x, start=c(-2, 2.4))
        stopifnot( equal(nlp$objval, 0) )
        stopifnot( equal(solution(nlp), c(1, 1)) )
    }
}

test.NLP_2 <- function() {
    f <- function(x) {
        return( 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2 )
    }

    f.gradient <- function(x) {
        return( c( -400 * x[1] * (x[2] - x[1] * x[1]) - 2 * (1 - x[1]),
                   200 * (x[2] - x[1] * x[1])) )
    }

    x <- OP( objective = F_objective(f, n=2L, G=f.gradient), 
             constraints = c(F_constraint(F=function(x) x[1] + x[2]^2, ">=", 0,
                                          J=function(x) c(1, 2*x[2])),
                             F_constraint(F=function(x) x[1]^2 + x[2], ">=", 0,
                                          J=function(x) c(2*x[1], x[2]))),
             bounds = V_bound(li=1:2, ui=1:2, lb=c(-2, -Inf), ub=c(0.5,  1)) )

    solver <- setdiff(ROI_applicable_solvers(x), "nlminb")
    if ( length(solver) ) {
        nlp <- ROI_solve(x, solver = solver[1L], start = c(-2, 1))
        stopifnot( equal(nlp$objval, 1/4) )
        stopifnot( equal(solution(nlp), c(1/2, 1/4)) )
    }
}

## ---------------------------
## test 
## ---------------------------
file = Sys.getenv("ROI_TEST_LOG_FILE")
ROI_TEST_ERRORS <- 0L
rt <- function(expr, silent = FALSE) {
    err <- try(expr, silent = silent)
    if ( inherits(err, "try-error") ) 
        ROI_TEST_ERRORS <<- ROI_TEST_ERRORS + 1L
    err
}

cat("# Constructors\n", file=file)
cat("## Objective\n", file=file)
cat("### L_objective\n", file=file)
rt( test.L_objective() )

cat("### Q_objective\n", file=file)
rt( test.Q_objective() )

cat("### F_objective\n", file=file)
rt( test.F_objective() )

cat("## Constraints\n", file=file)

cat("## Types\n", file=file)

cat("## Bounds\n", file=file)

cat("# R-Methods\n", file=file)

cat("## Combine\n", file=file)
cat("## Combine L_constraints\n", file=file)
rt( test.combine_L_constraints() )

cat("# Optimization Problems\n", file=file)
cat("## LP\n", file=file)
rt( test.LP() )

cat("## QP\n", file=file)
rt( test.QP() )

cat("## SOCP\n", file=file)
rt( test.SOCP_1() )
rt( test.SOCP_2() )

cat("## EXPP\n", file=file)
rt( test.EXPP_1() )
rt( test.EXPP_2() )
rt( test.EXPP_3() )

cat("## EXPD\n", file=file)
rt( test.EXPD_1() )

cat("## POWP\n", file=file)
rt( test.POWP_1() )

cat("## POWD\n", file=file)
rt( test.POWD_1() )

cat("## SDP\n", file=file)
rt( test.SDP_1() )

cat("## NLP\n", file=file)
rt( test.NLP_1() )
rt( test.NLP_2() )


if ( ROI_TEST_ERRORS ) {
    stop(ROI_TEST_ERRORS, " errors occured")
}

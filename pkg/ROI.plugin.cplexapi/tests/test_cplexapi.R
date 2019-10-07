if (FALSE) {
    q("no")
    R
}

Sys.setenv("ROI_LOAD_PLUGINS" = FALSE)
library(ROI)
library(ROI.plugin.cplexapi)

library(cplexAPI)

check <- function(domain, condition, level=1, message="", call=sys.call(-1L)) {
    if ( isTRUE(condition) ) return(invisible(NULL))
    msg <- sprintf("in %s", domain)
    if ( all(nchar(message) > 0) ) msg <- sprintf("%s\n\t%s", msg, message)
    stop(msg)
    return(invisible(NULL))
}

## LP - Example - 1
## max:  2 x_1 + 4 x_2 + 3 x_3
## s.t.
## 3 x_1  +  4 x_2  +  2 x_3  <= 60
## 2 x_1  +    x_2  +  2 x_3  <= 40
##   x_1  +  3 x_2  +  2 x_3  <= 80 
## x_1, x_2, x_3 >= 0
test_lp_01 <- function(solver) {
    mat <- matrix(c(3, 4, 2,
                    2, 1, 2,
                    1, 3, 2), nrow=3, byrow=TRUE)
    x <- OP(objective = c(2, 4, 3),
            constraints = L_constraint(L = mat,
                                       dir = c("<=", "<=", "<="),
                                       rhs = c(60, 40, 80)),
            maximum = TRUE)

    sol <- c(0, 20/3, 50/3)
    
    opt <- ROI_solve(x, solver = solver)
    check("LP-01@01", equal(opt$solution, sol, tol=1e-4))
    check("LP-01@02", equal(opt$objval, 230/3, tol=1e-4))
}

## Test if ROI can handle empty constraint matrix.
test_lp_02 <- function(solver) {
    x <- OP(objective = c(2, 4, 3),
            constraints = L_constraint(L=matrix(0, nrow=0, ncol=3), 
                                       dir=character(), rhs=double()),
            maximum = FALSE)

    sol <- c(0, 0, 0)

    opt <- ROI_solve(x, solver = solver)
    check("LP-02@01", equal(opt$solution, sol, tol=1e-4))
    check("LP-02@02", equal(opt$objval, 0, tol=1e-4))
}

## Test if ROI can handle when the constraint is equal to NULL.
test_lp_03 <- function(solver) {
    x <- OP(objective = c(2, 4, 3), constraints = NULL, maximum = FALSE)

    sol <- c(0, 0, 0)

    opt <- ROI_solve(x, solver = solver)
    check("LP-03@03", equal(opt$solution, sol, tol=1e-4))
    check("LP-03@03", equal(opt$objval, 0, tol=1e-4))
}

## MILP - Example - 1
## min:  3 x + 1 y + 3 z
## s.t.
##      -1 x  +    y  +   z  <=  4
##               4 y  - 3 z  <=  2
##         x  -  3 y  + 2 z  <=  3
##     x, z \in Z_+
##     y >= 0, z >= 2, x <= 4, y <= 100
test_milp_01 <- function(solver) {
    obj <- c(3, 1, 3)
    A <- rbind(c(-1,  2,  1),
               c( 0,  4, -3),
               c( 1, -3,  2))
    b <- c(4, 2, 3)
    bounds <- V_bound(li = c(1L, 3L), ui = c(1L, 2L),
                      lb = c(-Inf, 2), ub = c(4, 100))

    x <- OP(objective = obj,
         constraints = L_constraint(L = A,
                                    dir = c("<=", "<=", "<="),
                                    rhs = b),
         types = c("I", "C", "I"),
         bounds = bounds,
         maximum = TRUE)

    sol <- c(4, 2.5, 3)

    opt <- ROI_solve(x, solver = solver)
    
    check("MILP-01@01", all(A %*% opt$solution <= b))
    check("MILP-01@04", equal(opt$solution , sol, tol=1e-01))
}


## MILP - Example - 2
## min:  3 x + 1 y + 3 z
## s.t.
##      -1 x  +    y  +   z  <=  4
##               4 y  - 3 z  <=  2
##         x  -  3 y  + 2 z  <=  3
##     x, z \in Z_+
##     y >= 0
test_milp_02 <- function(solver) {
    obj <- c(3, 1, 3)
    A <- rbind(c(-1,  2,  1),
               c( 0,  4, -3),
               c( 1, -3,  2))
    b <- c(4, 2, 3)

    x <- OP(objective = obj,
         constraints = L_constraint(L = A,
                                    dir = c("<=", "<=", "<="),
                                    rhs = b),
         types = c("I", "C", "I"),
         maximum = TRUE)

    sol <- c(5, 2.75, 3)

    opt <- ROI_solve(x, solver = solver)
    check("MILP-02@01", all(A %*% opt$solution <= b))
    check("MILP-02@04", equal(opt$solution , sol, tol=1e-01))
}


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
test_qp_01 <- function(solver) {

    A <- cbind(c(-4, -3, 0), 
               c( 2,  1, 0), 
               c( 0, -2, 1))
    x <- OP(Q_objective(diag(3), L =  c(0, -5, 0)),
            L_constraint(L = t(A),
                         dir = rep(">=", 3),
                         rhs = c(-8, 2, 0)))

    opt <- ROI_solve(x, solver = solver)
    solution <- c(0.476190476190476, 1.04761904761905, 2.0952380952381)
    check("QP-01@01", equal(solution(opt), solution) )
    check("QP-01@02", equal(solution(opt, "objval"), -2.38095238095238) )

}


## This Test detects non-conform objective functions.
## minimize 0.5 x^2 - 2 x + y
## s.t. x <= 3
## Type 1:   0.5 x'Qx + c'Lx => c(2, 0)  objval=-2
## Type 2:       x'Qx + c'Lx => c(3, 0)  objval=-3.75
test_qp_02 <- function(solver) {

    zero <- .Machine$double.eps * 100
    qo <- Q_objective(Q=rbind(c(1, 0), c(0, zero)), L=c(-2, 1))
    lc1 <- L_constraint(L=matrix(c(1, 0), nrow = 1), dir = "<=", rhs = 3)
    lc2 <- L_constraint(L=matrix(c(1, 0), nrow = 1), dir = ">=", rhs = 0)
    x <- OP(qo, c(lc1, lc2))

    opt <- ROI_solve(x, solver = solver)
    solution <- c(2, 0)
    check("QP-02@01", equal(solution(opt), solution) )
    check("QP-02@02", equal(solution(opt, "objval"), -2) )

}

## as qp_01 but maximize
test_qp_03 <- function(solver) {
    A <- cbind(c(-4, -3, 0), 
               c( 2,  1, 0), 
               c( 0, -2, 1))
    x <- OP(Q_objective(-diag(3), L = -c(0, -5, 0)),
            L_constraint(L = t(A),
                         dir = rep(">=", 3),
                         rhs = c(-8, 2, 0)),
            maximum = TRUE)

    opt <- ROI_solve(x, solver = solver)
    solution <- c(0.476190476190476, 1.04761904761905, 2.0952380952381)
    check("QP-03@01", equal(opt$solution, solution) )
    check("QP-03@02", equal(opt$objval, 2.38095238095238) )
}

## Another QP (this is qpex1.c in the CPLEX examples)
## maximize:     x_1 + 2 x_2 + 3  x_3 - 1/2 (33 x_1^2 + 22 x_2^2 + 11 x_3^2) + 6 x_1 x_2 + 11.5 x_2 x_3
## subject to: - x_1 +   x_2 +    x_3 <= 20
##               x_1 - 3 x_2 +    x_3 <= 30
##
test_qp_04 <- function(solver) {
    Q0 <- matrix(c(-33, 6, 0, 6, -22, 11.5, 0, 11.5, -11), byrow = TRUE, ncol = 3)
    L0 <- c(1, 2, 3)
    L1 <- matrix(c(-1, 1, 1, 1, -3, 1), byrow = TRUE, ncol = 3)
    x <- OP(Q_objective(Q = Q0, L = L0),
            L_constraint(L = L1, dir = leq(2), rhs = c(20, 30)),
            maximum = TRUE)
    opt <- ROI_solve(x, solver = solver)    
    solution <- c(0.13911493553437, 0.598465474706656, 0.898395723872851)
    check("QP-04@01", equal(solution(opt), solution) )
    check("QP-04@02", equal(solution(opt, "objval"), 2.01561652328916) )    
}

## This test detects if each solver is using the same definition
## for quadratic constraints.
## minimize:    0.5 * (x^2 + y^2)
## subject to:  0.5 * x^2 >= 0.5
##      x, y >= 0
## solution <- c(1, 0)
test_qcqp_01 <- function(solver) {
    qo <- Q_objective(Q = diag(2), L =  numeric(2))
    qc <- Q_constraint(rbind(c(1, 0), c(0, 0)), c(0, 0), dir=">=", rhs=0.5)
    x <- OP(qo, qc)

    sol <- c(1, 0)

    opt <- ROI_solve(x, solver = solver)
   
    ## local_opts <- list( algorithm = "NLOPT_LD_LBFGS", xtol_rel  = 1e-4 )
    ## opt <- ROI_solve(x, solver="nloptr", start=c(2, 2), method="NLOPT_LD_MMA")

    check("QCQP-01@01", equal(opt$solution, sol) )
    check("QCQP-01@02", equal(opt$objval, 0.5) )
}

## QCP (this is qcpex1.c in the CPLEX examples)
## maximize:     x_1 + 2 x_2 + 3 x_3 - 1/2 (33 x_1^2 + 22 x_2^2 + 11 x_3^2) + 6 x_1 x_2 + 11.5 x_2 x_3
## subject to: - x_1 +   x_2 +   x_3   <= 20
##               x_1 - 3 x_2 +   x_3   <= 30
##               1/2 (2 x_1^2 + 2 x_2^2 + 2 x_3^2) <= 1
test_qcqp_02 <- function(solver) {
    Q0 <- matrix(c(-33, 6, 0, 6, -22, 11.5, 0, 11.5, -11), byrow = TRUE, ncol = 3)
    L0 <- c(1, 2, 3)
    QC <- list(NULL, NULL, diag(2, nrow = 3))
    LC <- matrix(c(-1, 1, 1, 1, -3, 1, 0, 0, 0), byrow = TRUE, ncol = 3)
    x <- OP(Q_objective(Q = Q0, L = L0),
            Q_constraint(Q = QC, L = LC, dir = leq(3), rhs = c(20, 30, 1)),
            maximum = TRUE)
    
    opt <- ROI_solve(x, solver = solver)#, method = "lpopt")

    solution <- c(0.12912360513025, 0.549952824880058, 0.825153905632591)
    check("QCQP-02@01", equal(solution(opt), solution) )
    check("QCQP-02@02", equal(solution(opt, "objval"), 2.00234664731505) )  
}

test_qcqp_03 <- function(solver) {
    Q0 <- matrix(c(-33, 6, 0, 6, -22, 11.5, 0, 11.5, -11), byrow = TRUE, ncol = 3)
    L0 <- c(1, 2, 3)
    QC <- list(NULL, NULL, diag(2, nrow = 3))
    LC <- matrix(c(-1, 1, 1, 1, -3, 1, 0, 0, 0), byrow = TRUE, ncol = 3)
    x <- OP(Q_objective(Q = Q0, L = L0),
            Q_constraint(Q = QC, L = LC, dir = leq(3), rhs = c(20, 30, 1)),
            types = c("B", "C", "I"),
            maximum = TRUE)
    
    opt <- ROI_solve(x, solver = solver)#, method = "lpopt")
    solution(opt)

    solution <- c(0, 0.090909094928918, 0)
    check("QCQP-02@01", equal(solution(opt), solution) )
    check("QCQP-02@02", equal(solution(opt, "objval"), 0.0909090909090907) )  
}


if ( !any("cplexapi" %in% names(ROI_registered_solvers())) ) {
    ## This should never happen.
    cat("ROI.plugin.cplexapi cloud not be found among the registered solvers.\n")
} else {
    solver <- "cplexapi"
    print("Start Testing!")
    local({test_lp_01(solver)})
    local({test_lp_02(solver)})
    local({test_lp_03(solver)})
    local({test_milp_01(solver)})
    local({test_milp_02(solver)})
    local({test_qp_01(solver)})
    local({test_qp_02(solver)})
    local({test_qp_03(solver)})
    local({test_qp_04(solver)})
    local({test_qcqp_01(solver)})
    local({test_qcqp_02(solver)})
    local({test_qcqp_03(solver)})
}




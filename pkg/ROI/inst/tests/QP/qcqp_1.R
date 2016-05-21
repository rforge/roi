context("QCQP")

##' QCQP - Example - 1
##' (this is qcpex1.c in the CPLEX examples)
##' maximize:     x_1 + 2 x_2 + 3 x_3 - 1/2 (33 x_1^2 + 22 x_2^2 + 11 x_3^2) + 6 x_1 x_2 + 11.5 x_2 x_3
##' subject to: - x_1 +   x_2 +   x_3   <= 20
##'               x_1 - 3 x_2 +   x_3   <= 30
##'               x_1^2 + x_2^2 + x_3^2 <= 1
##'
##' Solver: CPLEX
##' Solution: c(0.12912360513025, 0.549952824880058, 0.825153905632591)
##' Objective Value: 2.00234664731505

test_that("QCQP - Example 1", {

    cat("QCQP - Example 1\n")
    cat("TODO: add slam to CPLEX!\n")
    library( testthat )
    library( ROI )
    library( slam )

    x <- OP( Q_objective(Q = matrix(c(-33, 6, 0, 6, -22, 11.5, 0, 11.5, -11),
                         byrow = TRUE, ncol = 3),
                         L = c(1, 2, 3)),
             Q_constraint(Q = list(NULL, NULL, diag(1, nrow = 3)),
                          L = matrix(c(-1, 1, 1, 1, -3, 1, 0, 0, 0), byrow = TRUE, ncol = 3),
                          dir = rep("<=", 3),
                          rhs = c(20, 30, 1)),
             maximum = TRUE )

    if( length(OP_applicable_solver(x)) == 0 ) cat(testthat:::colourise("\tERROR no Applicable Solver!\n", "error"))

    for ( SOLVER in OP_applicable_solver(x) ) {
        cat("  ", SOLVER)
        opt <- ROI_solve(x, solver=SOLVER)
        solution <- c(0.12912360513025, 0.549952824880058, 0.825153905632591)
        check( opt$solution, solution )
        check( opt$objval, 2.00234664731505 )
        cat("\n")
    }

})

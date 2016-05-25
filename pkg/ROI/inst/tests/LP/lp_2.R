context("LP")

## LP - Example - 2
## maximize:    3 x_1 + 1 x_2 + 3 x_3
## subject to: -1 x_1 + 2 x_2 +   x_3 <= 4
##                      4 x_2 - 3 x_3 <= 2
##                x_1 - 3 x_2 + 2 x_3 <= 3
##                x_1, x_3 are non-negative integers
##                x_2 is a non-negative real number
##
## solution: c(5, 2.75, 3)
## objective value: 26.75

test_that("LP Example 2", {
    mat <- matrix(c(-1, 0, 1, 2, 4, -3, 1, -3, 2), nrow = 3)
    lp <- OP(objective = c(3, 1, 3),
             constraints = L_constraint(L = mat,
                                        dir = c("<=", "<=", "<="),
                                        rhs = c(4, 2, 3)),
             types = c("I", "C", "I"),
             maximum = TRUE)

    opt <- ROI_solve(lp, solver = SOLVER)
    expect_that( equal(opt$solution, c(5, 2.75, 3)), equals( TRUE ) )
    expect_that( equal(opt$objval, 26.75), equals( TRUE ) )
} )

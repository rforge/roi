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
    lp <- OP(objective = c(2, 4, 3),
             constraints = L_constraint(L = mat,
                                        dir = c("<=", "<=", "<="),
                                        rhs = c(60, 40, 80)),
             maximum = TRUE)

    opt <- ROI_solve(lp, solver = solver)
    check("LP-01@01", equal(opt$solution, c(0, 20/3, 50/3), tol=1e-4))
    check("LP-01@02", equal(opt$objval, 230/3, tol=1e-4))
}

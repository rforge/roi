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
    
    opt <- ROI_solve(x, solver = solver, 
                     control = solver_control(solver, sol))
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

    opt <- ROI_solve(x, solver = solver, 
                     control = solver_control(solver, sol))
    check("LP-02@01", equal(opt$solution, sol, tol=1e-4))
    check("LP-02@02", equal(opt$objval, 0, tol=1e-4))
}

## Test if ROI can handle when the constraint is equal to NULL.
test_lp_03 <- function(solver) {
    x <- OP(objective = c(2, 4, 3), constraints = NULL, maximum = FALSE)

    sol <- c(0, 0, 0)

    opt <- ROI_solve(x, solver = solver, 
                     control = solver_control(solver, sol))
    check("LP-03@03", equal(opt$solution, sol, tol=1e-4))
    check("LP-03@03", equal(opt$objval, 0, tol=1e-4))
}



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

    opt <- ROI_solve(x, solver = solver, 
                     control = solver_control(solver, sol))
    
    check("MILP-01@01", all(A %*% opt$solution <= b))
    check("MILP-01@02", correct_types(x, solution(opt)))
    check("MILP-01@03", check_bounds(x, solution(opt)))
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

    opt <- ROI_solve(x, solver = solver, 
                     control = solver_control(solver, sol))
    check("MILP-02@01", all(A %*% opt$solution <= b))
    check("MILP-01@02", correct_types(x, solution(opt)))
    check("MILP-01@03", check_bounds(x, solution(opt)))
    check("MILP-02@04", equal(opt$solution , sol, tol=1e-01))
}


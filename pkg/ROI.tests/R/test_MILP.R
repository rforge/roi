## MILP - Example - 1
## min:  3 x + 1 y + 3 z
## s.t.
##      -1 x  +    y  +   z  <=  4
##               4 y  - 3 z  <=  2
##         x  -  3 y  + 2 z  <=  3
##     x, z \in Z_+
##     y >= 0
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

    control <- list()
    if ( solver == "ecos" ) {
        tol <- 1e-03
        mi_tol <- 1e-01
        control$MAXIT <- 2000L
        control$FEASTOL <- tol
        control$RELTOL <- tol
        control$ABSTOL <- tol
        control$MI_MAX_ITERS <- 5000L
        control$MI_INT_TOL <- mi_tol
        control$MI_ABS_EPS <- mi_tol
        control$MI_REL_EPS <- mi_tol
        control$VERBOSE <- 0L
    }
    opt <- ROI_solve(x, solver=solver, control=control)
    
    check("MILP-01@01", equal(opt$solution , c(4, 2.5, 3), tol=1e-01))
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

    control <- list()
    if ( solver == "ecos" ) {
        tol <- 1e-03
        mi_tol <- 1e-01
        control$MAXIT <- 2000L
        control$FEASTOL <- tol
        control$RELTOL <- tol
        control$ABSTOL <- tol
        control$MI_MAX_ITERS <- 5000L
        control$MI_INT_TOL <- mi_tol
        control$MI_ABS_EPS <- mi_tol
        control$MI_REL_EPS <- mi_tol
        control$VERBOSE <- 0L
    }

    opt <- ROI_solve(x, solver=solver, control=control)
    check("MILP-02@01", all(A %*% opt$solution <= b))
    check("MILP-02@02", equal(opt$solution , c(5, 2.75, 3), tol=1e-01))
}


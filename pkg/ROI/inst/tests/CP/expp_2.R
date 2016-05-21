context("EXPP")

## EXPP - Example - 1
## max:  x + y + z
## s.t.
## y e^(x/y) <= z
## y > 0
## y == 2
## z == 2 * exp(1/2)
## c(1, 2, 2*exp(1/2))
test_that("Example 2", {

    cat("EXPP Example 2\n")
    library( testthat )
    library( ROI )

    obj <- c(1, 1, 1)
    A <- rbind(c(0, 1, 0),
               c(0, 0, 1))
    b <- c(2, 2*exp(1/2))
    G <- diag(x=-1, 3)
    h <- rep(0, 3)
    cones <- list("free"=c(1, 2), "expp"=list(3:5))
    bound <- as.C_bound(cones)

    x <- OP(objective = obj,
            constraints = L_constraint(L = rbind(A, G), dir=rep("==", length(c(b, h))), rhs = c(b, h)),
            types = rep("C", 3),
            bounds =  bound,
            maximum = TRUE)

    for ( SOLVER in OP_applicable_solver(x) ) {
        cat("  ", SOLVER)
        opt <- ROI_solve(x, solver=SOLVER, control=list(eps=1e-12))
        check( opt$solution , c(1, 2, 2*exp(1/2)) )
        cat("\n")
    }   

} )

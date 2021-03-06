context("EXPD")

## EXPD - Example - 1
## min:  u + v + w
## s.t.
## 		-u * e^(v/u) <= e * w; u < 0; v >= 0; w >= 0
##      u == -1
##      v ==  1
##
##      c(-1, 1, exp(-2))
test_that("Example 1", {

    cat("EXPD Example 1\n")
    library( testthat )
    library( ROI )

    obj <- c(1, 1, 1)
    A <- rbind(c(1,  0, 0),
               c(0,  1, 0))
    b <- c(-1, 1)
    G <- diag(x=-1, 3)
    h <- rep(0, 3)
    cones <- list("free"=c(1, 2), "expd"=list(3:5))
    bound <- as.C_bound(cones)

    x <- OP(objective = obj,
            constraints = L_constraint(L = rbind(A, G), dir=rep("==", length(c(b, h))), rhs = c(b, h)),
            types = rep("C", 3),
            bounds =  bound,
            maximum = FALSE)

    for ( SOLVER in OP_applicable_solver(x) ) {
        cat("  ", SOLVER)      
        opt <- ROI_solve(x, solver=SOLVER, control=list(eps=1e-12))
        check( opt$solution , c(-1, 1, exp(-2)) )
        cat("\n")
    }

} )

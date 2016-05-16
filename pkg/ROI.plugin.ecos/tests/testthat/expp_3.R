context("EXPP")

## EXPP - Example - 3
## max:  x + y + z
## s.t.
## y e^(x/y) <= z
## y > 0
## y == 1
## z == exp(1)
## c(1, 1, exp(1))
test_that("Example 3", {

    if ( identical(parent.frame(), globalenv()) ) {
        library( testthat )
        library( ROI )
    }

    obj <- c(1, 1, 1)
    A <- rbind(c(0, 1, 0),
               c(0, 0, 1))
    b <- c(1, exp(1))
    G <- diag(x=-1, 3)
    h <- rep(0, 3)
    cones <- list("free"=c(1, 2), "expp"=list(3:5))
    bound <- as.C_bound(cones)

    x <- OP(objective = obj,
            constraints = L_constraint(L = rbind(A, G), dir=rep("==", length(c(b, h))), rhs = c(b, h)),
            types = rep("C", 3),
            bounds =  bound,
            maximum = TRUE)

    opt <- ROI_solve(x, solver=SOLVER, control=list(eps=1e-12))
    expect_equal( opt$solution , c(1, 1, exp(1)) )

} )
